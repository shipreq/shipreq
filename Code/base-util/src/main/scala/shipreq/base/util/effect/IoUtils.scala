package shipreq.base.util.effect

import scalaz.effect.IO
import scalaz.syntax.bind._
import shipreq.base.util.ErrorOr

object IoUtils {

  val nop: IO[Unit] = IO(())

  val clockMs = IO{ System.currentTimeMillis }

  implicit class IoExt[A](val io: IO[A]) extends AnyVal {
    @inline def tap(f: A => IO[_]): IO[A] = io.flatMap(a => f(a) >> IO(a))
    @inline def <| (f: A => IO[_]): IO[A] = io tap f

    @inline def castError[B](implicit ev: A =:= ErrorOr[Nothing]): IO[ErrorOr[B]] = io.asInstanceOf[IO[ErrorOr[B]]]
  }

  def time[A](io: IO[A]): IO[(Long, A)] =
    for {
      start <- clockMs
      a     <- io
      end   <- clockMs
    } yield (end - start, a)

  def time_[A](io: IO[A])(log: A => Long => IO[Unit]): IO[A] =
    for {
      start <- clockMs
      a     <- io
      end   <- clockMs
      _     <- log(a)(end - start)
    } yield a
}
