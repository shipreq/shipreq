package shipreq.webapp.base.protocol

import scalaz.Equal
import boopickle.Pickler

object Routine {

  /**
   * Description of a server-side routine (function).
   */
  abstract class Desc {
    type I
    type O
    implicit def pi: Pickler[I]
    implicit def po: Pickler[O]
    final type Remote = Routine.Remote[this.type]
  }

  /** Syntactic convenience that allows for single-line declaration. */
  abstract class =>|=>[_I, _O](implicit I: Pickler[_I], O: Pickler[_O]) extends Desc {
    final override type I = _I
    final override type O = _O
    final override implicit def pi = I
    final override implicit def po = O
  }

  type Aux[_I, _O] = Desc {type I = _I; type O = _O}

  /**
   * A routine ready for remote invocation.
   *
   * @param n The server-side Lift function key.
   */
  final case class Remote[D <: Desc](n: String, d: D)

  implicit def equalDesc[D <: Desc]: Equal[D] = Equal.equalRef
  implicit def equalRemove[D <: Desc]: Equal[Remote[D]] =
    Equal.equal((a, b) => a.n == b.n && (a.d eq b.d))
}