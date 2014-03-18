package shipreq.base.util

import scalaz.{-\/, \/-}
import scalaz.\&/.{Both, That, This}

object ErrorOr {
  def apply[A](a: A): ErrorOr[A] = \/-(a)

  def catchException[A](a: => ErrorOr[A]): ErrorOr[A] =
    try a catch { case e: Throwable => Error(e) }

  def catchExceptionO[A](a: => Option[ErrorOr[A]]): Option[ErrorOr[A]] =
    try a catch { case e: Throwable => Some(Error(e)) }

  def annotate[A](ann: => String)(eoa: ErrorOr[A]): ErrorOr[A] =
    eoa.leftMap(Error.annotate(ann, _))

  def annotateO[A](ann: => String)(o: Option[ErrorOr[A]]): Option[ErrorOr[A]] =
    o.map(annotate(ann))

  def require_![A](eoa: ErrorOr[A]): A =
    eoa match {
      case \/-(v) => v
      case -\/(e) => throw Error.throwable(e)
    }
}

object Error {
  def apply[A](m: String)              : ErrorOr[A] = -\/(This(m))
  def apply[A](e: Throwable)           : ErrorOr[A] = -\/(That(e))
  def apply[A](m: String, e: Throwable): ErrorOr[A] = -\/(Both(m, e))

  def error[A](m: String)              : Error = This(m)
  def error[A](e: Throwable)           : Error = That(e)
  def error[A](m: String, e: Throwable): Error = Both(m, e)

  @inline private[this] def merge(a: String, b: String): String =
    s"$a -- $b"

  def msg(error: Error): String = error match {
    case This(m)    => m
    case That(e)    => e.getMessage
    case Both(m, e) => merge(m, e.getMessage)
  }

  def annotate(a: String, error: Error): Error = error match {
    case This(m)    => This(merge(a, m))
    case That(e)    => Both(a, e)
    case Both(m, e) => Both(merge(a, m), e)
  }

  def throwable(error: Error): Throwable = error match {
    case This(m)    => new RuntimeException(m)
    case That(e)    => e
    case Both(m, e) => new RuntimeException(m, e)
  }
}
