package shipreq.webapp.client.util

import japgolly.scalajs.react._, ScalazReact._, MonocleReact._
import monocle.Lens
import scalaz.effect.IO

/**
 * External variable.
 */
final class EVar[A](val value: A, val set: A => IO[Unit]) {

  def mod(f: A => A): A => IO[Unit] =
    a => set(f(a))

  def setL[B](f: Lens[A, B]): B => IO[Unit] =
    b => set(f.set(b)(value))
}

object EVar {
  @inline def apply[A](value: A)(set: A => IO[Unit]): EVar[A] =
    new EVar(value, set)

  @inline def overState[S]($: ComponentStateFocus[S]): EVar[S] =
    new EVar($.state, $.setStateIO(_))
}