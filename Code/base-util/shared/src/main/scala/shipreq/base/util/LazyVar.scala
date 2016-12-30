package shipreq.base.util

import scalaz.effect.IO

/**
  * Lazy variable.
  *
  * NOT thread-safe.
  */
final class LazyVar[A](thunk: () => A) {

  private[this] var thunkRef = thunk
  private[this] var value: A = _

  def initialised: Boolean =
    thunkRef eq null

  def get: IO[A] =
    IO {
    if (!initialised)
      set(thunkRef())
    value
  }

  def set(a: A): IO[Unit] =
    IO {
    thunkRef = null
    value = a
  }

  def mod(f: A => A): IO[Unit] =
    for {
      a1 <- get
      a2 = f(a1)
      _ <- set(a2)
    } yield ()

  def modIO(f: A => IO[A]): IO[Unit] =
    for {
      a1 <- get
      a2 <- f(a1)
      _ <- set(a2)
    } yield ()
}

object LazyVar {
  def apply[A](a: => A): LazyVar[A] =
    new LazyVar(() => a)

  def io[A](a: => IO[A]): LazyVar[A] =
    apply(a.unsafePerformIO())
}