package shipreq.webapp.base.protocol

import boopickle.Pickler
import japgolly.univeq._
import shipreq.base.util.EqualsByRef
import BoopickleMacros._
import BinCodecGeneric.stringPickler

/** Identifier of a server-side procedure, generated and provided by the server. */
final case class ServerSideProcId(value: String)
object ServerSideProcId {
  @inline implicit def univEq: UnivEq[ServerSideProcId] = UnivEq.derive
  implicit val pickler: Pickler[ServerSideProcId] = pickleCaseClass
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/** An instance of a server-side procedure, (available for invocation). */
final case class ServerSideProc[I, O](id: ServerSideProcId, protocol: ServerSideProc.Protocol[I, O])

object ServerSideProc {

  @inline implicit def univEq[I, O]: UnivEq[ServerSideProc[I, O]] =
    UnivEq.derive

  /**
    * The protocol by which a server-side procedure can be invoked.
    *
    * Input => Failure \/ Output
    */
  class Protocol[I, O](final val name: String)(implicit pi: Pickler[I], po: Pickler[O]) extends EqualsByRef {
    final type Input = I
    final type Output = O
    final implicit val pickleInput : Pickler[I] = pi
    final implicit val pickleOutput: Pickler[O] = po

    final type Instance = ServerSideProc[I, O]

    final implicit val pickleInstance: Pickler[Instance] =
      xmap[Instance, ServerSideProcId](ServerSideProc(_, this))(_.id)
  }

  object Protocol {
    def apply[I: Pickler, O: Pickler](name: String): Protocol[I, O] =
      new Protocol(name)
  }
}
