package shipreq.webapp.client.util

import scalaz.effect.IO

final class Refreshable[I, V](f: I => V, val value: V) {
  def refresh(i: I): Refreshable[I, V] =
    new Refreshable(f, f(i))

  def asVar = new RefreshableVar(this)
}

object Refreshable {
  def apply[I, V](f: I => V)(initialInput: I): Refreshable[I, V] =
    new Refreshable(f, f(initialInput))

  def thunk[V](f: => V): Refreshable[Unit, V] =
    new Refreshable(_ => f, f)
}

final class RefreshableVar[I, V](init: Refreshable[I, V]) {
  @volatile private var v = init

  def value: V = v.value

  def refresh(i: => I): IO[Unit] =
    IO{v = v.refresh(i)}
}