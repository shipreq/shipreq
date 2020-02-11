package shipreq.base.util

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import japgolly.univeq.UnivEq
import java.net.URL
import scala.annotation.tailrec
import scalaz.Order
import scalaz.std.anyVal.intInstance
import scala.collection.GenTraversable
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.TreeMap
import scala.util.Try
import ScalaExt.StringBuilderExt

object Util {

  def quickJSB(f: java.lang.StringBuilder => Unit): String = {
    val sb = new java.lang.StringBuilder
    f(sb)
    sb.toString
  }

  def quickJSB(start: String, f: java.lang.StringBuilder => Unit): String = {
    val sb = new java.lang.StringBuilder(start)
    f(sb)
    sb.toString
  }

  def quickSB(f: StringBuilder => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }

  def quickSB(start: String, f: StringBuilder => Unit): String = {
    val sb = new StringBuilder(start)
    f(sb)
    sb.toString
  }

  def quickToString(clz: Class[_])(fs: (StringBuilder => Any)*): String =
    quickSB(clz.getSimpleName, _.mkStringF("(", ", ", ")")(fs: _*))

  private[this] val simpleClassNameRegex = "^.+[\\.\\$]".r

  // https://issues.scala-lang.org/browse/SI-2034
  def simpleName(c: Class[_]): String =
    simpleClassNameRegex.replaceFirstIn(c.getName, "")

  val simpleNameMemo: Class[_] => String =
    Memo(simpleName)

  def existentLocalResources(paths: List[String]): Stream[URL] =
    paths.toStream.map(f => Try(getClass.getResource(f)).getOrElse(null)).filter(_ ne null)

  def cutoffStr(s: String, cutoff: Int): String =
    if (s.length <= cutoff)
      s
    else
      s.substring(0, cutoff - 1) + "\u2026"

  def foldAndIndex[A, B, K](as: TraversableOnce[A], z: B, ik: Int => K)(f: (B, K, A) => B): (B, Map[K, A]) = {
    var i = 0
    var m = Map.empty[K, A]
    var b = z
    as.foreach { a =>
      val k = ik(i)
      b = f(b, k, a)
      m = m.updated(k, a)
      i += 1
    }
    (b, m)
  }

  def foldAndIndexI[A, B](as: TraversableOnce[A], z: B)(f: (B, Int, A) => B): (B, Map[Int, A]) =
    foldAndIndex(as, z, identity[Int])(f)

  def foldAndIndexS[A, B](as: TraversableOnce[A], z: B)(f: (B, String, A) => B): (B, Map[String, A]) =
    foldAndIndex(as, z, i => (i + 33).toChar.toString)(f)

  def indexI[A](as: TraversableOnce[A]): Map[Int, A] =
    foldAndIndexI(as, ())((_, _, _) => ())._2

  def indexS[A](as: TraversableOnce[A]): Map[String, A] =
    foldAndIndexS(as, ())((_, _, _) => ())._2

  def filterAndSortByName[A](as: GenTraversable[A])(f: A => Boolean, name: A => String): Iterable[A] =
    as.foldLeft(TreeMap.empty[String, A])((q, a) =>
      if (f(a))
        q.updated(name(a), a)
      else
        q
    ).values

  @inline final def filterOutAndSortByName[A](as: GenTraversable[A])(f: A => Boolean, name: A => String): Iterable[A] =
    filterAndSortByName(as)(!f(_), name)

  //def fix[A, B <: A, C >: A](before: B, after: A)(test: A => Boolean, fix: A => C): C =
  def fixBeforeAfter[A](before: A, after: A)(test: A => Boolean, fix: A => A): A =
    if (test(before) && !test(after))
      fix(after)
    else
      after

  @inline def maybeUse[A](allow: Boolean)(ok: => Stream[A]): Stream[A] =
    if (allow) ok else Stream.empty

  @inline def maybeAdd[A](have: Stream[A], allow: Boolean)(add: => A): Stream[A] =
    if (allow) add #:: have else have

  def nextElement[A: UnivEq](as: Vector[A])(a: A): A =
    as((as.indexOf(a) + 1) % as.length)

  def mapReduce[A, B](as: TraversableOnce[A], whenEmpty: => B)(map: A => B, reduce: (B, B) => B): B =
    mapReduceB(as, whenEmpty)(map, map)(reduce)

  def mapReduceB[A, B, C](as: TraversableOnce[A], whenEmpty: => C)(mapFirst: A => C, map: A => B)(reduce: (C, B) => C): C = {
    val it = as.toIterator
    if (it.hasNext) {
      var r = mapFirst(it.next())
      while (it.hasNext)
        r = reduce(r, map(it.next()))
      r
    } else
      whenEmpty
  }

  /**
   * Fit an index within the acceptable window of indices.
   *
   * @param i The index needing correction.
   * @param length The length of the collection. Must be > 0.
   * @return 0 ≤ new-index < length
   */
  def fitCollectionIndex(i: Int, length: Int): Int = {
    assert(length > 0, "Length must be > 0, got: " + length)
    var j = i
    while (j >= length)
      j %= length
    while (j < 0)
      j += length
    j
  }

  def univEqAndArbitraryOrder[A](values: Iterable[A]): UnivEq[A] with Order[A] = {
    val fixedOrder = values.zipWithIndex.toMap
    new UnivEq[A] with Order[A] {
      @inline private[this] def int(s: A) = fixedOrder(s)
      override def order(a: A, b: A) = Order[Int].order(int(a), int(b))
      override def equal(a: A, b: A) = a == b
    }
  }

  def sideBySideStrings(str1: String, str2: String, sep: String = " | "): String =
    sideBySideStringSeqs(str1 split '\n', str2 split '\n', sep) mkString "\n"

  def sideBySideStringSeqs(vec1: IndexedSeq[String], vec2: IndexedSeq[String], sep: String = " | "): Vector[String] = {
    def get(x: IndexedSeq[String], i: Int): String =
      if (i < x.length) x(i) else ""
    val fmt = s"%-${(vec1 :+ "").iterator.map(_.length).max}s%s%s"
    val b = Vector.newBuilder[String]
    for (i <- 0 until vec1.length.max(vec2.length)) {
      val x = get(vec1, i)
      val y = get(vec2, i)
      b += fmt.format(x, sep, y)
    }
    b.result()
  }

  def partitionBetween[F[x] <: Traversable[x], A](as: F[A])(split: (A, A) => Boolean)
                                                 (implicit cbf: CanBuildFrom[Nothing, A, F[A]]): (F[A], F[A]) =
    if (as.isEmpty)
      (as, as)
    else {
      val b1, b2 = cbf()
      val it = as.toIterator
      @tailrec def go(prev: A): Unit =
        if (it.hasNext) {
          val a = it.next()
          if (split(prev, a)) {
            b2 += a
            b2 ++= it
          } else {
            b1 += a
            go(a)
          }
        }

      val first = it.next()
      b1 += first
      go(first)
      (b1.result(), b2.result())
    }

  def partitionConsecutive[F[x] <: Traversable[x], A](as: F[A])(implicit cbf: CanBuildFrom[Nothing, A, F[A]], n: Numeric[A]): (F[A], F[A]) =
    partitionConsecutiveBy(as)(identity)

  def partitionConsecutiveBy[F[x] <: Traversable[x], A, B](as: F[A])(f: A => B)
                                                          (implicit cbf: CanBuildFrom[Nothing, A, F[A]], n: Numeric[B]): (F[A], F[A]) =
    partitionBetween(as)((a, b) => !n.equiv(n.plus(f(a), n.one), f(b)))

  def uniqueDupsNested[A, B: UnivEq](as: TraversableOnce[A])(bs: A => TraversableOnce[B]): Set[B] = {
    var uniq = Set.empty[B]
    var dups = Set.empty[B]
    for {
      a <- as
      b <- bs(a)
    }
      if (!uniq.contains(b))
        // first sighting
        uniq += b
      else if (!dups.contains(b)) {
        // second sighting
        dups += b
      }
    dups
  }

  def mergeSets[A: UnivEq](x: Set[_ <: A], y: Set[_ <: A]): Set[A] =
    if (x.isEmpty) y.asInstanceOf[Set[A]]
    else if (y.isEmpty) x.asInstanceOf[Set[A]]
    else x ++ y

  def mergeSets[A: UnivEq](x: Set[_ <: A], y: Set[_ <: A], z: Set[_ <: A]): Set[A] =
    if (x.isEmpty) mergeSets(y, z)
    else if (y.isEmpty) mergeSets(x, z)
    else if (z.isEmpty) mergeSets(x, y)
    else (Set.newBuilder[A] ++= x ++= y ++= z).result()

  def enumOrdering[A: UnivEq, B: Ordering](as: TraversableOnce[A])(by: A => B): Ordering[A] = {
    val sorted = MutableArray(as).sortBySchwartzian(by).array
    val ord = sorted.iterator.mapToOrder
    Ordering.by(ord.apply)
  }

  /** Returns a prefix of a string such that it's utf-8 encoding doesn't exceed a given number of bytes. */
  def limitUtf8Bytes(s: String, maxBytes: Int): String = {
    // Taken from https://stackoverflow.com/a/119586/1846272
    var b = 0
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      var skip = 0
      var more = 0
      if (c <= 0x007f)
        more = 1
      else if (c <= 0x07FF)
        more = 2
      else if (c <= 0xd7ff)
        more = 3
      else if (c <= 0xDFFF) {
        // surrogate area, consume next char as well
        more = 4
        skip = 1
      } else
        more = 3
      if (b + more > maxBytes)
        return s.substring(0, i)
      b += more
      i += skip
      i += 1
    }
    s
  }

  // TODO Move into microlibs
  def unindentBy(str: String, spaces: Int): String = {
    if (spaces <= 0)
      return str

    var smallest = spaces

    val lines =
      str.linesWithSeparators.map { line =>
        if (line.stripLineEnd.nonEmpty) {
          val indent = line.iterator.takeWhile(_ == ' ').size
          if (indent == 0)
            return str
          if (indent < smallest)
            smallest = indent
        }
        line
      }.toArray

    if (smallest == Int.MaxValue)
      return str

    lines.iterator.map { line =>
      if (line.stripLineEnd.nonEmpty)
        line.drop(smallest)
      else
        line
    }.mkString("")
  }
}
