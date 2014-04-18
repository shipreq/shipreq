package shipreq.base.util

import scalaz.syntax.bind._
import scalaz.effect.IO

package object effect {

  // ===================================================================================================================
  // IO

  val nopIo: IO[Unit] = IO(())

  implicit class IOExt[A](val io: IO[A]) extends AnyVal {
    @inline def tap(f: A => IO[_]): IO[A] = io.flatMap(a => f(a) >> IO(a))
    @inline def <| (f: A => IO[_]): IO[A] = io tap f

    @inline def castError[B](implicit ev: A =:= ErrorOr[Nothing]): IO[ErrorOr[B]] = io.asInstanceOf[IO[ErrorOr[B]]]
  }

  // ===================================================================================================================
  // IOE

  type IOE[A] = IO[ErrorOr[A]]

  object IOE {
    @inline def apply[A](f: => A): IOE[A] = IO(ErrorOr safe f)

    @inline def error[A](e: Error)               : IOE[A] = IO(e.toErrorOr)
    @inline def error[A](m: String)              : IOE[A] = error(Error(m))
    @inline def error[A](e: Throwable)           : IOE[A] = error(Error(e))
    @inline def error[A](m: String, e: Throwable): IOE[A] = error(Error(m, e))

    val nop = apply(())
  }

}
