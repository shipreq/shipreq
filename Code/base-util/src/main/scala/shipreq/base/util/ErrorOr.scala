package shipreq.base.util

import scalaz.{Applicative, -\/, \/-, \&/, Lens}
import scalaz.\&/.{Both, That, This}

object ErrorOr {
  def apply[A](a: A): ErrorOr[A] = \/-(a)

  def catchException[A](a: => ErrorOr[A]): ErrorOr[A] =
    try a catch { case e: Throwable => Error(e) }

  def catchExceptionM[M[_], A](a: => M[ErrorOr[A]])(implicit M: Applicative[M]): M[ErrorOr[A]] =
    try a catch { case e: Throwable => M.point(Error(e)) }

  def annotate[A](ann: => String)(eoa: ErrorOr[A]): ErrorOr[A] =
    eoa.leftMap(Error.annotate(ann, _))

  def annotateO[A](ann: => String)(o: Option[ErrorOr[A]]): Option[ErrorOr[A]] =
    o.map(annotate(ann))

  def tag[A](t: => ErrorTag)(eoa: ErrorOr[A]): ErrorOr[A] =
    eoa.leftMap(_ tag t)

  def require_![A](eoa: ErrorOr[A]): A =
    eoa match {
      case \/-(v) => v
      case -\/(e) => throw Error.throwable(e)
    }
}

trait ErrorTag

final case class Error(reason: String \&/ Throwable, tags: Set[ErrorTag] = Set.empty) {
  def tag(t: ErrorTag) = Error(reason, tags + t)
  def is(t: ErrorTag): Boolean = tags contains t
}

object Error {
  val reasonLens = Lens.lensg[Error, String \&/ Throwable](e => r => Error(r, e.tags)  , _.reason)
  val tagsLens   = Lens.lensg[Error, Set[ErrorTag]       ](e => t => Error(e.reason, t), _.tags)

  @inline final def apply[A](m: String)              : ErrorOr[A] = -\/(error(m))
  @inline final def apply[A](e: Throwable)           : ErrorOr[A] = -\/(error(e))
  @inline final def apply[A](m: String, e: Throwable): ErrorOr[A] = -\/(error(m, e))

  @inline final def error[A](m: String)              : Error = Error(This(m))
  @inline final def error[A](e: Throwable)           : Error = Error(That(e))
  @inline final def error[A](m: String, e: Throwable): Error = Error(Both(m, e))

  private[this] def merge(a: String, b: String): String =
    if (a.isEmpty) b
    else if (b.isEmpty) a
    else {
      val l: Int = a.last
      val p = if (Character.isLetterOrDigit(l)) ". "
         else if (Character.isSpaceChar(l)) ""
         else " "
      a + p + b
    }

  def msg(error: Error): String = error.reason match {
    case This(m)    => m
    case That(e)    => e.getMessage
    case Both(m, e) => merge(m, e.getMessage)
  }

  def annotate(a: String, error: Error): Error =
    reasonLens.mod({
      case This(m)    => This(merge(a, m))
      case That(e)    => Both(a, e)
      case Both(m, e) => Both(merge(a, m), e)
    }, error)

  def throwable(error: Error): Throwable = error.reason match {
    case This(m)    => new RuntimeException(m)
    case That(e)    => e
    case Both(m, e) => new RuntimeException(m, e)
  }
}
