package shipreq.base.util

import scalaz.Scalaz.Id
import scalaz.{Applicative, -\/, \/-, \&/, Lens}
import scalaz.\&/.{Both, That, This}

object ErrorOr {
  def apply[A](a: A): ErrorOr[A] = \/-(a)

  def catchException[A](a: => ErrorOr[A]): ErrorOr[A] =
    catchExceptionM[Id, A](a)

  def catchExceptionM[M[_], A](a: => M[ErrorOr[A]])(implicit M: Applicative[M]): M[ErrorOr[A]] =
    try a catch {
      case ErrorAsThrowable(e) => M.point(e.toErrorOr)
      case e: Throwable        => M.point(Error(e))
    }

  def annotate[A](ann: => String)(eoa: ErrorOr[A]): ErrorOr[A] =
    eoa.leftMap(_ annotate ann)

  def annotateM[M[_], A](ann: => String)(a: M[ErrorOr[A]])(implicit M: Applicative[M]): M[ErrorOr[A]] =
    M.map(a)(annotate(ann))

  def catchAndAnnotate[A](ann: => String)(a: => ErrorOr[A]): ErrorOr[A] =
    annotate(ann)(catchException(a))

  def catchAndAnnotateM[M[_], A](ann: => String)(a: => M[ErrorOr[A]])(implicit M: Applicative[M]): M[ErrorOr[A]] =
    annotateM(ann)(catchExceptionM(a))

  def tag[A](t: => ErrorTag)(eoa: ErrorOr[A]): ErrorOr[A] =
    eoa.leftMap(_ tag t)

  def tagM[M[_], A](t: => ErrorTag)(m: M[ErrorOr[A]])(implicit M: Applicative[M]): M[ErrorOr[A]] =
    M.map(m)(_.leftMap(_ tag t))

  def catchAndTag[A](t: => ErrorTag)(a: => ErrorOr[A]): ErrorOr[A] =
    tag(t)(catchException(a))

  def catchAndTagM[M[_], A](t: => ErrorTag)(a: => M[ErrorOr[A]])(implicit M: Applicative[M]): M[ErrorOr[A]] =
    tagM(t)(catchExceptionM(a))

  def safe[A](a: => A): ErrorOr[A] =
    catchException(ErrorOr(a))

  def safeA[A](ann: => String)(a: => A): ErrorOr[A] =
    catchAndAnnotate(ann)(ErrorOr(a))

  def safeT[A](t: => ErrorTag)(a: => A): ErrorOr[A] =
    catchAndTag(t)(ErrorOr(a))

  def require_![A](eoa: ErrorOr[A]): A =
    eoa match {
      case \/-(v) => v
      case -\/(e) => e.throw_!()
    }
}

trait ErrorTag

final case class Error(reason: String \&/ Throwable, tags: Set[ErrorTag] = Set.empty) {
  import Error._

  def annotate(a: String): Error =
    reasonLens.mod({
      case This(m)    => This(merge(a, m))
      case That(e)    => Both(a, e)
      case Both(m, e) => Both(merge(a, m), e)
    }, this)

  def tag(t: ErrorTag) =
    Error(reason, tags + t)

  def is(t: ErrorTag): Boolean =
    tags contains t

  def throw_!(): Nothing =
    throw throwable

  def toErrorOr[A]: ErrorOr[A] =
    -\/(this)

  def msg: String = reason match {
    case This(m)    => m
    case That(e)    => e.getMessage
    case Both(m, e) => merge(m, e.getMessage)
  }

  def cause: Option[Throwable] = reason match {
    case This(_)    => None
    case That(e)    => Some(e)
    case Both(_, e) => Some(e)
  }

  def throwable: Throwable = reason match {
    case This(_)    => ErrorAsThrowable(this)
    case That(e)    => e
    case Both(_, e) => ErrorAsThrowable(this)
  }
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

  private def merge(a: String, b: String): String =
    if (a.isEmpty) b
    else if (b.isEmpty) a
    else {
      val l: Int = a.last
      val p = if (Character.isLetterOrDigit(l)) ". "
         else if (Character.isSpaceChar(l)) ""
         else " "
      a + p + b
    }
}

final case class ErrorAsThrowable(e: Error) extends RuntimeException(e.msg, e.cause getOrElse null)
