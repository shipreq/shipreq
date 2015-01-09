package shipreq.base.util

import java.net.URL
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

  def foldAndIndex[A, B](as: TraversableOnce[A], z: B)(f: (B, Int, A) => B): (B, Map[Int, A]) = {
    var i = 0
    var m = Map.empty[Int, A]
    var b = z
    as.foreach { a =>
      b = f(b, i, a)
      m = m.updated(i, a)
      i += 1
    }
    (b, m)
  }

}

object ParseLong {
  def unapply(s: String): Option[Long] =
    try {
      Some(s.toLong)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
}