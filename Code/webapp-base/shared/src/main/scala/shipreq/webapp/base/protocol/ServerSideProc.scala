package shipreq.webapp.base.protocol

import boopickle.Pickler
import japgolly.univeq._
import shipreq.base.util.EqualsByRef
import BoopickleMacros.xmap
import BinCodecGeneric.stringPickler

/** An instance of a server-side procedure, (available for invocation). */
final case class ServerSideProc[I, O](key: String, protocol: ServerSideProc.Protocol[I, O])

object ServerSideProc {

  @inline implicit def univEq[I, O]: UnivEq[ServerSideProc[I, O]] =
    UnivEq.derive

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /**
    * The protocol by which a server-side procedure can be invoked.
    *
    * Input => Failure \/ Output
    */
  class Protocol[I, O](implicit pi: Pickler[I], po: Pickler[O]) extends EqualsByRef {
    final type Input = I
    final type Output = O
    final implicit val pickleInput : Pickler[I] = pi
    final implicit val pickleOutput: Pickler[O] = po

    final type Instance = ServerSideProc[I, O]

    final implicit val pickleInstance: Pickler[Instance] =
      xmap[Instance, String](ServerSideProc(_, this))(_.key)
  }

  object Protocol {
    def apply[I: Pickler, O: Pickler]: Protocol[I, O] =
      new Protocol
  }

}
