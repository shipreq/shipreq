package shipreq.base.util

import java.net.URL
import scala.collection.GenTraversable
import scala.collection.immutable.TreeMap
import scala.util.Try
import scalaz.{Equal, Memo}
import scalaz.syntax.equal._
import ScalaExt.StringBuilderExt

object Util {

  @inline def quickSB(f: StringBuilder => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }

  @inline def quickSB(start: String, f: StringBuilder => Unit): String = {
    val sb = new StringBuilder(start)
    f(sb)
    sb.toString
  }

  @inline def quickToString(clz: Class[_])(fs: (StringBuilder => Any)*): String =
    quickSB(clz.getSimpleName, _.mkStringF("(", ", ", ")")(fs: _*))

  private[this] val simpleClassNameRegex = "^.+[\\.\\$]".r

  // https://issues.scala-lang.org/browse/SI-2034
  def simpleName(c: Class[_]): String =
    simpleClassNameRegex.replaceFirstIn(c.getName, "")

  val simpleNameMemo =
    Memo.immutableHashMapMemo[Class[_], String](simpleName _)

  def existentLocalResources(paths: List[String]): Stream[URL] =
    paths.toStream.map(f => Try(getClass.getResource(f)).getOrElse(null)).filter(_ ne null)

  def cutoffStr(s: String, cutoff: Int): String =
    if (s.length <= cutoff)
      s
    else
      s.substring(0, cutoff - 1) + "\u2026"

  /** Inserts `subj` immediately before a given element, or appends if `before` is None. */
  def reposition[A: Equal](v: Vector[A], subj: A, before: Option[A]): Vector[A] =
    before match {
      case Some(b) =>
        v.foldLeft(Vector.empty[A])((q, e) => {
          val q2 = if (e ≟ b) q :+ subj else q
          if (e ≟ subj) q2 else q2 :+ e
        })
      case None =>
        v.filterNot(_ ≟ subj) :+ subj
    }

  /**
   * Dual of `reposition()`.
   * @return The element immediately following the given subject.
   */
  def position[A: Equal](v: Vector[A], subj: A): Option[A] = {
    val i = v.indexWhere(subj ≟ _)
    if (i >= 0 && (i + 1) < v.length) Some(v(i + 1)) else None
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
}

object ParseLong {
  def unapply(s: String): Option[Long] =
    try {
      Some(s.toLong)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
}

object ParseInt {
  def unapply(s: String): Option[Int] =
    try {
      Some(s.toInt)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
}