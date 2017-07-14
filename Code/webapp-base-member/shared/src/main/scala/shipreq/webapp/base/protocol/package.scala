package shipreq.webapp.base

package object protocol {
  import boopickle.Pickler
  import scalaz.\/
  import BinCodecGeneric.pickleXor
  import shipreq.webapp.base.event.VerifiedEvent
  import BinCodecEvents._

  implicit val pickleErrorMsgOrVerifiedEventSeq: Pickler[ErrorMsg \/ VerifiedEvent.Seq] =
    pickleXor(ErrorMsg.pickleErrorMsg, pickleVerifiedEventSeq)

  implicit class MemberExt_ServerSideProcProtocol(private val self: ServerSideProc.Protocol.type) extends AnyVal {
    import ServerSideProc.Protocol._

    def toEvents[I: Pickler]: Aux[ErrorMsg, I, VerifiedEvent.Seq] =
      lowLevel[ErrorMsg, I, VerifiedEvent.Seq]
  }


}
