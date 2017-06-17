package shipreq.webapp.base.protocol

import boopickle.Pickler
import japgolly.univeq._
import scalaz.\/
import shipreq.base.util.EqualsByRef
import shipreq.webapp.base.event.VerifiedEvents
import BoopickleMacros.xmap
import BinCodecGeneric.{pickleXor, stringPickler}
import BinCodecEvents.pickleVerifiedEvents

/** An instance of a server-side procedure, (available for invocation). */
sealed trait ServerSideProc {

  /** The server-side Lift function key. */
  val key: String

  val protocol: ServerSideProc.Protocol

  final override def hashCode() = key.hashCode
  final override def equals(obj: Any): Boolean =
    obj match {
      case o: ServerSideProc => (key ==* o.key) && (protocol ==* o.protocol)
      case _                 => false
    }
}

object ServerSideProc {

  type For[P <: Protocol] = ServerSideProc {val protocol: P}

  def apply(_key: String, p: Protocol): For[p.type] =
    new ServerSideProc {
      override val key = _key
      override val protocol: p.type = p
    }

  @inline implicit def univEq[P <: ServerSideProc]: UnivEq[P] =
    UnivEq.force

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /**
    * The protocol by which a server-side procedure can be invoked.
    *
    * Input => Failure \/ Output
    */
  abstract class Protocol private[protocol]() extends EqualsByRef {
    type Failure
    type Input
    type Output

    final type Response = Failure \/ Output

    implicit val pickleFailure : Pickler[Failure]
    implicit val pickleInput   : Pickler[Input]
    implicit val pickleOutput  : Pickler[Output]
    implicit val pickleResponse: Pickler[Response]

    final type Instance = ServerSideProc.For[this.type]

    final implicit val pickleInstance: Pickler[Instance] =
      xmap[Instance, String](ServerSideProc(_, this))(_.key)
  }

  object Protocol {
    type Aux[F, I, O] = Protocol {type Failure = F; type Input = I; type Output = O}

    // Everything just uses ErrorMsg at the moment
    private[protocol] def apply[I: Pickler, O: Pickler]: Aux[ErrorMsg, I, O] = {
      implicit val r = BinCodecGeneric.pickleXor[ErrorMsg, O]
      _apply[ErrorMsg, I, O]
    }

    private def _apply[F, I, O](implicit f: Pickler[F], i: Pickler[I], o: Pickler[O], r: Pickler[F \/ O]): Aux[F, I, O] =
      new Protocol {
        override type Input   = I
        override type Output  = O
        override type Failure = F
        override implicit val pickleInput    = i
        override implicit val pickleOutput   = o
        override implicit val pickleFailure  = f
        override implicit val pickleResponse = r
      }

    implicit val pickleErrorMsgOrVerifiedEvents: Pickler[ErrorMsg \/ VerifiedEvents] =
      pickleXor(ErrorMsg.pickleErrorMsg, pickleVerifiedEvents)

    def toEvents[I: Pickler]: Aux[ErrorMsg, I, VerifiedEvents] =
      _apply[ErrorMsg, I, VerifiedEvents]
  }

}
