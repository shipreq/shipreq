package shipreq.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq.UnivEq
import java.net.URL
import scalaz.Order
import scalaz.std.anyVal.intInstance
import scala.collection.GenTraversable
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

  def deleteVectorElement[A](v: Vector[A], n: Int): Vector[A] = {
    val b = v.companion.newBuilder[A]
    var i = 0
    v.foreach{ x => if (i != n) b += x; i += 1 }
    b.result()
  }

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

  /**
   * Pattern.quote doesn't work in Scala.JS.
   *
   * http://stackoverflow.com/questions/2593637/how-to-escape-regular-expression-in-javascript
   */
  def regexEscape(s: String): String = {
    var r = s
    r = regexEscape1.replaceAllIn(r, """\\$1""")
    r = regexEscape2.replaceAllIn(r, """\\x08""")
    r
  }

  private[this] val regexEscape1 = """([-()\[\]{}+?*.$\^|,:#<!\\])""".r
  private[this] val regexEscape2 = """\x08""".r

  def regexEscapeAndWrap(s: String): String =
    s"(?:${regexEscape(s)})"

  //def fix[A, B <: A, C >: A](before: B, after: A)(test: A => Boolean, fix: A => C): C =
  def fixBeforeAfter[A](before: A, after: A)(test: A => Boolean, fix: A => A): A =
    if (test(before) && !test(after))
      fix(after)
    else
      after

  /**
   * Space = Θ(mn)
   * Time  = Θ(nᵐ)
   */
  def levenshtein(str1: String, str2: String): Int = {
    val m = str1.length
    val n = str2.length

    val d: Array[Array[Int]] = Array.ofDim(m + 1, n + 1)
    for (i <- 0 to m) d(i)(0) = i
    for (j <- 0 to n) d(0)(j) = j

    for (i <- 1 to m; j <- 1 to n) {
      val cost = if (str1(i - 1) == str2(j - 1)) 0 else 1
      val a = d(i-1)(j  ) + 1     // deletion
      val b = d(i  )(j-1) + 1     // insertion
      val c = d(i-1)(j-1) + cost  // substitution
      d(i)(j) = a min b min c
    }

    d(m)(n)
  }

  @inline def maybeUse[A](allow: Boolean)(ok: => Stream[A]): Stream[A] =
    if (allow) ok else Stream.empty

  @inline def maybeAdd[A](have: Stream[A], allow: Boolean)(add: => A): Stream[A] =
    if (allow) add #:: have else have

  def nextElement[A: UnivEq](as: Vector[A])(a: A): A =
    as((as.indexOf(a) + 1) % as.length)

  def mapReduce[A, B](as: Iterable[A], whenEmpty: => B)(map: A => B, reduce: (B, B) => B): B =
    mapReduceB(as, whenEmpty)(map, map)(reduce)

  def mapReduceB[A, B, C](as: Iterable[A], whenEmpty: => C)(mapFirst: A => C, map: A => B)(reduce: (C, B) => C): C = {
    val it = as.iterator
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

  def togglePresence[A](as: Set[A])(a: A): Set[A] =
    if (as contains a)
      as - a
    else
      as + a

  def mapToOrder[A](as: TraversableOnce[A]): Map[A, Int] =
    as.toIterator.zipWithIndex.toMap

  def univEqAndArbitraryOrder[A](values: Iterable[A]): UnivEq[A] with Order[A] = {
    val fixedOrder = values.zipWithIndex.toMap
    new UnivEq[A] with Order[A] {
      @inline private[this] def int(s: A) = fixedOrder(s)
      override def order(a: A, b: A) = Order[Int].order(int(a), int(b))
      override def equal(a: A, b: A) = a == b
    }
  }

  def quickStringExists(strings: Set[String]): String => Boolean = {
    val maxLen = strings.foldLeft(0)(_ max _.length)
    val byLen = Array.fill(maxLen + 1)(Set.empty[String])
    strings.foreach(s => byLen(s.length) = byLen(s.length) + s)
    s => (s.length <= maxLen) && byLen(s.length).contains(s)
  }

  // Improved a larger benchmark by 8.6%
  def quickStringLookup[A](map: Map[String, A]): String => Option[A] = {
    val strings = map.keySet
    val maxLen = strings.foldLeft(0)(_ max _.length)
    val byLen = Array.fill(maxLen + 1)(Map.empty[String, A])
    strings.foreach(s => byLen(s.length) = byLen(s.length).updated(s, map(s)))
    s => if (s.length <= maxLen) byLen(s.length).get(s) else None
  }

  def dups[A: UnivEq](as: TraversableOnce[A]): Iterator[A] = { // TODO Move to microlibs
    val seen = collection.mutable.HashSet.empty[A]
    as.toIterator.map { a =>
      if (seen contains a)
        Some(a)
      else {
        seen += a
        None
      }
    }.filterDefined
  }
}
