package shipreq.webapp.base

import shipreq.base.util.ErrorMsg

package object protocol {
  import boopickle.Pickler
  import scalaz.\/
  import shipreq.webapp.base.event.VerifiedEvent
  import BinCodecGeneric._
  import BinCodecBaseData._
  import BinCodecEvents._

  implicit val pickleErrorMsgOrVerifiedEventSeq: Pickler[ErrorMsg \/ VerifiedEvent.Seq] =
    pickleXor(picklerErrorMsg, pickleVerifiedEventSeq)

  implicit class MemberExt_ServerSideProcProtocol(private val self: ServerSideProc.Protocol.type) extends AnyVal {
    def toEvents[I: Pickler](name: String): ServerSideProc.Protocol[I, ErrorMsg \/ VerifiedEvent.Seq] =
      ServerSideProc.Protocol(name)
  }

}
