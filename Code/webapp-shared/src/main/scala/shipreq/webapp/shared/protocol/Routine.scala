package shipreq.webapp.shared.protocol

object Routine {

  /**
   * Description of a remote routine.
   */
  abstract class Desc {
    type I
    type O
    final type Remote = Routine.Remote[this.type]
  }

  /** Syntactic convenience that allows for single-line declaration. */
  abstract class DescT[I_, O_] extends Desc {
    final override type I = I_
    final override type O = O_
  }

  /**
   * Descriptor of a remotely available routine.
   * @param n The server-side Lift function key.
   */
  case class Remote[D <: Desc](n: String, d: D)

  // TODO Customise serialisation for Remote
  // {"square":{"n":"F751737835735KSD2LY","d":{}} should just be "F751737835735KSD2LY"

  /** Denotes a set of routines. */
  trait Group
}