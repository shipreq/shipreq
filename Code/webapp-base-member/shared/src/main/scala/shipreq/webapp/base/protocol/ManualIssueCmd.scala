package shipreq.webapp.base.protocol

import boopickle.Pickler
import shipreq.webapp.base.data.ManualIssueId
import shipreq.webapp.base.text.Text

sealed trait ManualIssueCmd

object ManualIssueCmd {
  import BoopickleMacros._
  import BinCodecMemberData.AtomPicklers.instances.manualIssueN
  import BinCodecMemberData._

  final case class Create(text: Text.ManualIssue.NonEmptyText) extends ManualIssueCmd

  final case class Update(id: ManualIssueId, text: Text.ManualIssue.NonEmptyText) extends ManualIssueCmd

  final case class Delete(id: ManualIssueId) extends ManualIssueCmd

  implicit val pickler: Pickler[ManualIssueCmd] = derivePickler
}
