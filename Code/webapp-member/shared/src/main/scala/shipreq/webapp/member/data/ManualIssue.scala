package shipreq.webapp.member.data

import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.webapp.member.text.Text
import shipreq.webapp.member.text.Text.Equality._

final case class ManualIssueId(value: Int) extends TaggedInt

final case class ManualIssue(id: ManualIssueId, text: Text.ManualIssue.NonEmptyText) {

  lazy val tags: Set[ApplicableTagId] =
    text.iterator.collect {
      case t: Text.ManualIssue.TagRef => t.value
    }.toSet
}

object ManualIssue {
  type IMap = shipreq.base.util.IMap[ManualIssueId, ManualIssue]

  val emptyIMap: IMap =
    shipreq.base.util.IMap.empty(_.id)

  implicit def univEq: UnivEq[ManualIssue] = UnivEq.derive
}

// =====================================================================================================================

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