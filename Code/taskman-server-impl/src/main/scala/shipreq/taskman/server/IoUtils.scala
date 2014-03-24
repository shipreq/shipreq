package shipreq.taskman.server

import scalaz.effect.IO

object IoUtils {

  val clockMs = IO{ System.currentTimeMillis }

  def time[A](io: IO[A]): IO[(Long, A)] =
    for {
      start <- clockMs
      a     <- io
      end   <- clockMs
    } yield (end - start, a)

  def timeU[A](io: IO[A])(f: A => Long => IO[Unit]): IO[A] =
    for {
      start <- clockMs
      a     <- io
      end   <- clockMs
      _     <- f(a)(end - start)
    } yield a
}
