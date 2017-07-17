package shipreq.base.util

import FxModule._

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

  def get: Fx[A] =
    Fx {
      if (!initialised)
        set(thunkRef()).unsafeRun()
      value
    }

  def set(a: A): Fx[Unit] =
    Fx {
      thunkRef = null
      value = a
    }

  def mod(f: A => A): Fx[Unit] =
    for {
      a1 <- get
      a2 = f(a1)
      _ <- set(a2)
    } yield ()

  def modFx(f: A => Fx[A]): Fx[Unit] =
    for {
      a1 <- get
      a2 <- f(a1)
      _ <- set(a2)
    } yield ()
}

object LazyVar {
  def apply[A](a: => A): LazyVar[A] =
    new LazyVar(() => a)

  def io[A](a: => Fx[A]): LazyVar[A] =
    apply(a.unsafeRun())
}