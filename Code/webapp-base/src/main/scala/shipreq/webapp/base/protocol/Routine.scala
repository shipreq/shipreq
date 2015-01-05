package shipreq.webapp.base.protocol

import upickle.{Reader, Writer}

object Routine {

  /**
   * Description of a remote routine.
   */
  abstract class Desc {
    type I
    type O
    implicit def ri: Reader[I]
    implicit def wi: Writer[I]
    implicit def ro: Reader[O]
    implicit def wo: Writer[O]
    final type Remote = Routine.Remote[this.type]
  }

  /** Syntactic convenience that allows for single-line declaration. */
  abstract class DescT[_I, _O](implicit RI: Reader[_I], WI: Writer[_I], RO: Reader[_O], WO: Writer[_O]) extends Desc {
    final override type I = _I
    final override type O = _O
    final override implicit def ri = RI
    final override implicit def wi = WI
    final override implicit def ro = RO
    final override implicit def wo = WO
  }

  /**
   * Descriptor of a remotely available routine.
   * @param n The server-side Lift function key.
   */
  final case class Remote[D <: Desc](n: String, d: D)
}