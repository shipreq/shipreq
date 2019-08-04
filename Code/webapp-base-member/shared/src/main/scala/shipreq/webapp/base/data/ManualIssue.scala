package shipreq.webapp.base.data

import japgolly.univeq.UnivEq
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.text.Text.Equality._

final case class ManualIssueId(value: Int) extends TaggedInt

final case class ManualIssue(id: ManualIssueId, text: Text.ManualIssue.NonEmptyText)

object ManualIssue {
  type IMap = shipreq.base.util.IMap[ManualIssueId, ManualIssue]

  val emptyIMap: IMap =
    shipreq.base.util.IMap.empty(_.id)

  implicit def univEq: UnivEq[ManualIssue] = UnivEq.derive
}

final case class ManualIssues(imap  : ManualIssue.IMap,
                              nextId: ManualIssueId) {

  def add(txt: Text.ManualIssue.NonEmptyText): ManualIssues = {
    val mi  = ManualIssue(nextId, txt)
    val mis = ManualIssues(imap + mi, ManualIssueId(nextId.value + 1))
    mis
  }

  def modIMap(f: ManualIssue.IMap => ManualIssue.IMap): ManualIssues =
    ManualIssues(f(imap), nextId)
}

object ManualIssues {

  def empty: ManualIssues =
    apply(ManualIssue.emptyIMap, ManualIssueId(1))
  
  implicit def univEq: UnivEq[ManualIssues] = UnivEq.derive
}