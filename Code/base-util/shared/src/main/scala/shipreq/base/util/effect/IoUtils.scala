package shipreq.base.util.effect

import scalaz.{-\/, \/, \/-}
import scalaz.effect.IO
import scalaz.syntax.bind._
import shipreq.base.util.ErrorOr

object IoUtils {

  val nop: IO[Unit] =
    IO.ioUnit

  val clockMs: IO[Long] =
    IO(System.currentTimeMillis())

  implicit class IoExt[A](private val self: IO[A]) extends AnyVal {
    @inline def tap(f: A => IO[_]): IO[A] = self.flatMap(a => f(a) >> IO(a))
    @inline def <| (f: A => IO[_]): IO[A] = self tap f

    /** Cos >> is screwed in Scalaz */
    @inline def >>>(next: => IO[Unit]): IO[Unit] = self.flatMap(_ => next)

    @inline def castError[B](implicit ev: A =:= ErrorOr[Nothing]): IO[ErrorOr[B]] =
      self.asInstanceOf[IO[ErrorOr[B]]]

    /** First continue arg = number of retries before now */
    def retryOnException(continue: (Int, Throwable) => Option[IO[Unit]]): IO[A] =
      self.catchLeft.retry(identity)(continue).map(_.fold(throw _, identity))

    /** First continue arg = number of retries before now */
    def retry[E, B](inspect: A => E \/ B)(continue: (Int, E) => Option[IO[Unit]]): IO[E \/ B] =
      IO.tailrecM((n: Int) =>
        self.flatMap[Int \/ (E \/ B)](a =>
          inspect(a) match {
            case \/-(b) => IO(\/-(\/-(b)))
            case -\/(e) =>
              continue(n, e) match {
                case None => IO(\/-(-\/(e)))
                case Some(x) => x.map(_ => -\/(n + 1))
              }
          })
      )(0)
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
