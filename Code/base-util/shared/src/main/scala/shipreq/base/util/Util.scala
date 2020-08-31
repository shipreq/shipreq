package shipreq.base.util

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import java.net.URL
import scala.collection.immutable.TreeMap
import scala.collection.{Factory, Iterable}
import scala.reflect.ClassTag
import scala.util.Try
import scalaz.std.anyVal.intInstance
import scalaz.{Applicative, Equal, Order}
import shipreq.base.util.ScalaExt.StringBuilderExt

object Util {

  // Immutable maps are optimised at low values to not even create a hash map
  def memoWithMapVar[K: UnivEq, V](f: K => V): K => V = { // TODO Add to microlibs
    var m = Map.empty[K, V]
    k =>
      if (m.contains(k))
        m(k)
      else {
        val v = f(k)
        m = m.updated(k, v)
        v
      }
  }

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

  def existentLocalResources(paths: List[String]): LazyList[URL] =
    paths.iterator.map(f => Try(getClass.getResource(f)).getOrElse(null)).filter(_ ne null).to(LazyList)

  def cutoffStr(s: String, cutoff: Int): String =
    if (s.length <= cutoff)
      s
    else
      s.substring(0, cutoff - 1) + "\u2026"

  def foldAndIndex[A, B, K](as: IterableOnce[A], z: B, ik: Int => K)(f: (B, K, A) => B): (B, Map[K, A]) = {
    var i = 0
    var m = Map.empty[K, A]
    var b = z
    as.iterator.foreach { a =>
      val k = ik(i)
      b = f(b, k, a)
      m = m.updated(k, a)
      i += 1
    }
    (b, m)
  }

  def foldAndIndexI[A, B](as: IterableOnce[A], z: B)(f: (B, Int, A) => B): (B, Map[Int, A]) =
    foldAndIndex(as, z, identity[Int])(f)

  def foldAndIndexS[A, B](as: IterableOnce[A], z: B)(f: (B, String, A) => B): (B, Map[String, A]) =
    foldAndIndex(as, z, i => (i + 33).toChar.toString)(f)

  def indexI[A](as: IterableOnce[A]): Map[Int, A] =
    foldAndIndexI(as, ())((_, _, _) => ())._2

  def indexS[A](as: IterableOnce[A]): Map[String, A] =
    foldAndIndexS(as, ())((_, _, _) => ())._2

  def filterAndSortByName[A](as: Iterable[A])(f: A => Boolean, name: A => String): Iterable[A] =
    as.foldLeft(TreeMap.empty[String, A])((q, a) =>
      if (f(a))
        q.updated(name(a), a)
      else
        q
    ).values

  @inline final def filterOutAndSortByName[A](as: Iterable[A])(f: A => Boolean, name: A => String): Iterable[A] =
    filterAndSortByName(as)(!f(_), name)

  //def fix[A, B <: A, C >: A](before: B, after: A)(test: A => Boolean, fix: A => C): C =
  def fixBeforeAfter[A](before: A, after: A)(test: A => Boolean, fix: A => A): A =
    if (test(before) && !test(after))
      fix(after)
    else
      after

  def nextElement[A: UnivEq](as: Vector[A])(a: A): A =
    as((as.indexOf(a) + 1) % as.length)

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
    sideBySideStringSeqs(
      ArraySeq unsafeWrapArray str1.split('\n'),
      ArraySeq unsafeWrapArray str2.split('\n'),
      sep)
      .mkString("\n")

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

  def partitionBetween[F[x] <: Iterable[x], A](as: F[A])(split: (A, A) => Boolean)
                                                 (implicit cbf: Factory[A, F[A]]): (F[A], F[A]) =
    if (as.isEmpty)
      (as, as)
    else {
      val b1, b2 = cbf.newBuilder
      val it = as.iterator
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

  def partitionConsecutive[F[x] <: Iterable[x], A](as: F[A])(implicit cbf: Factory[A, F[A]], n: Numeric[A]): (F[A], F[A]) =
    partitionConsecutiveBy(as)(identity)

  def partitionConsecutiveBy[F[x] <: Iterable[x], A, B](as: F[A])(f: A => B)
                                                          (implicit cbf: Factory[A, F[A]], n: Numeric[B]): (F[A], F[A]) =
    partitionBetween(as)((a, b) => !n.equiv(n.plus(f(a), n.one), f(b)))

  def uniqueDupsNested[A, B: UnivEq](as: IterableOnce[A])(bs: A => IterableOnce[B]): Set[B] = {
    var uniq = Set.empty[B]
    var dups = Set.empty[B]
    for {
      a <- as.iterator
      b <- bs(a).iterator
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

  def mergeMaps[K, V](x: Map[K, V], y: Map[K, V]): Map[K, V] =
         if (x.isEmpty) y
    else if (y.isEmpty) x
    else x ++ y

  def mergeSets[A: UnivEq](x: Set[_ <: A], y: Set[_ <: A]): Set[A] =
         if (x.isEmpty) y.asInstanceOf[Set[A]]
    else if (y.isEmpty) x.asInstanceOf[Set[A]]
    else x ++ y

  def mergeSets[A: UnivEq](x: Set[_ <: A], y: Set[_ <: A], z: Set[_ <: A]): Set[A] =
         if (x.isEmpty) mergeSets(y, z)
    else if (y.isEmpty) mergeSets(x, z)
    else if (z.isEmpty) mergeSets(x, y)
    else (Set.newBuilder[A] ++= x ++= y ++= z).result()

  def mergeSets[A: UnivEq](w: Set[_ <: A], x: Set[_ <: A], y: Set[_ <: A], z: Set[_ <: A]): Set[A] =
         if (w.isEmpty) mergeSets(x, y, z)
    else if (x.isEmpty) mergeSets(w, y, z)
    else if (y.isEmpty) mergeSets(w, x, z)
    else if (z.isEmpty) mergeSets(w, x, y)
    else (Set.newBuilder[A] ++= w ++= x ++= y ++= z).result()

  def mergeSets[A: UnivEq](v: Set[_ <: A], w: Set[_ <: A], x: Set[_ <: A], y: Set[_ <: A], z: Set[_ <: A]): Set[A] =
         if (v.isEmpty) mergeSets(w, x, y, z)
    else if (w.isEmpty) mergeSets(v, x, y, z)
    else if (x.isEmpty) mergeSets(v, w, y, z)
    else if (y.isEmpty) mergeSets(v, w, x, z)
    else if (z.isEmpty) mergeSets(v, w, x, y)
    else (Set.newBuilder[A] ++= v ++= w ++= x ++= y ++= z).result()

  def enumOrdering[A: UnivEq, B: Ordering](as: IterableOnce[A])(by: A => B): Ordering[A] = {
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

  def separate(input: String, g: String => Int): Vector[String \/ String] = {
    val b = Vector.newBuilder[String \/ String]
    var i = 0
    var nongap = 0
    def takeNonGap(): Unit =
      if (nongap != 0) {
        val ng = input.substring(i - nongap, i)
        b += \/-(ng)
        nongap = 0
      }

    while (i < input.length) {
      val s = input.drop(i)
      val gapSize = g(s)
      if (gapSize == 0) {
        nongap += 1
        i += 1
      } else {
        takeNonGap()
        val gap = s.take(gapSize)
        b += -\/(gap)
        i += gapSize
      }
    }
    takeNonGap()
    b.result()
  }

  def separateByWhitespaceOrCommas(input: String): Vector[String \/ String] =
    separate(input, _.takeWhile(c => c == ',' || c.isWhitespace).length)

  def vectorConcatDistinct[A](x: Vector[A], y: Vector[A])(implicit e: Equal[A]): Vector[A] =
    if (x eq y)
      x
    else if (x.isEmpty)
      y
    else
      y.foldLeft(x)((q, a) =>
        if (x.exists(e.equal(_, a))) q else q :+ a)

  def arraySeqConcatDistinct[A](x: ArraySeq[A], y: ArraySeq[A])(implicit e: Equal[A]): ArraySeq[A] =
    if (x eq y)
      x
    else if (x.isEmpty)
      y
    else
      y.foldLeft(x)((q, a) =>
        if (x.exists(e.equal(_, a))) q else q :+ a)

  implicit class ShipReqOpsForArraySeq[A](private val self: ArraySeq[A]) extends AnyVal {

    def splitOn(sep: A)(implicit e: Equal[A]): ArraySeq[ArraySeq[A]] = {
      val b = ArraySeq.newBuilder[ArraySeq[A]]

      @tailrec
      def go(rem: ArraySeq[A]): Unit =
        if (rem.nonEmpty)
          rem.indexWhere(e.equal(sep, _)) match {
            case -1 =>
              b += rem
            case n =>
              b += rem.take(n)
              go(rem.drop(n + 1))
          }

      go(self)
      b.result()
    }

    def traverse[G[_], B: ClassTag](f: A => G[B])(implicit G: Applicative[G]): G[ArraySeq[B]] = {
      if (self.isEmpty)
        G.pure(ArraySeq.empty[B])
      else {
        val gh = f(self.head)
        val gz = G.map(gh)(_ => ArraySeq.empty[B])
        val gt = self.tail.foldLeft(gz)((q, a) => G.apply2(q, f(a))(_ :+ _))
        G.apply2(gh, gt)(_ +: _)
      }
    }
  }

  def countOccurrences(str: String, subString: String): Int = {
    @tailrec def count(pos: Int, c: Int): Int = {
      val idx = str.indexOf(subString, pos)
      if (idx == -1)
        c
      else
        count(idx + subString.length, c + 1)
    }
    count(0, 0)
  }
}
