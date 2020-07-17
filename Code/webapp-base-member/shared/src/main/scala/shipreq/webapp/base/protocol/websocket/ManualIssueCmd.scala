package shipreq.webapp.base.protocol.websocket

import shipreq.webapp.base.data.ManualIssueId
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.text.Text.Equality._

sealed trait ManualIssueCmd

object ManualIssueCmd {

  final case class Create(text: Text.ManualIssue.NonEmptyText) extends ManualIssueCmd

  final case class Update(id: ManualIssueId, text: Text.ManualIssue.NonEmptyText) extends ManualIssueCmd

  final case class Delete(id: ManualIssueId) extends ManualIssueCmd

  implicit def univEq: UnivEq[ManualIssueCmd] = UnivEq.derive

  // ===================================================================================================================
  object CodecsV2 {
    import boopickle.DefaultBasic._
    import shipreq.webapp.base.protocol.binary.v1.BaseMemberData1._
    import shipreq.webapp.base.protocol.binary.v1.Rev3.AtomPicklers.instances.manualIssueN
    // REMEMBER: Don't forget to increment `CodecsVn` if you change these

    private implicit val picklerCreate: Pickler[Create] =
      transformPickler(Create.apply)(_.text)

    private implicit val picklerUpdate: Pickler[Update] =
      new Pickler[Update] {
        override def pickle(a: Update)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.text)
        }
        override def unpickle(implicit state: UnpickleState): Update = {
          val id   = state.unpickle[ManualIssueId]
          val text = state.unpickle[Text.ManualIssue.NonEmptyText]
          Update(id, text)
        }
      }

    private implicit val picklerDelete: Pickler[Delete] =
      transformPickler(Delete.apply)(_.id)

    implicit val picklerManualIssueCmd: Pickler[ManualIssueCmd] =
      new Pickler[ManualIssueCmd] {
        private[this] final val KeyCreate = 'c'
        private[this] final val KeyDelete = 'd'
        private[this] final val KeyUpdate = 'u'
        override def pickle(a: ManualIssueCmd)(implicit state: PickleState): Unit =
          a match {
            case b: Create => state.enc.writeByte(KeyCreate); state.pickle(b)
            case b: Delete => state.enc.writeByte(KeyDelete); state.pickle(b)
            case b: Update => state.enc.writeByte(KeyUpdate); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): ManualIssueCmd =
          state.dec.readByte match {
            case KeyCreate => state.unpickle[Create]
            case KeyDelete => state.unpickle[Delete]
            case KeyUpdate => state.unpickle[Update]
          }
      }
  }
}
