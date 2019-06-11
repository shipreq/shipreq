package shipreq.webapp.base.issue

import japgolly.univeq.UnivEq
import shipreq.webapp.base.data._

sealed trait IssueCategory
object IssueCategory {
  case object BadData     extends IssueCategory
  case object Futility    extends IssueCategory
  case object MissingData extends IssueCategory
  case object UserDefined extends IssueCategory

  implicit def univEq: UnivEq[IssueCategory] = UnivEq.derive
}

sealed abstract class Issue(final val category: IssueCategory)
object Issue {
  import IssueCategory._

  // BadData
  // =======
  // Deleted issue tag in use: #TBD
  // Deleted tag in use: #backend
  // Reference to deleted data

  final case class ConflictingTags(reqId: ReqId, tagGroupId: TagGroupId) extends Issue(BadData)

//  final case class DeadCustomIssueInUse(req: Req, issue: CustomIssueType) extends Issue(BadData)
//  final case class DeadTagInUse(req: Req, tag: ApplicableTag) extends Issue(BadData)

  // Futility
  // ========
  // Code group has nothing to group.
  // Status field has no tags


  // MissingData
  // ===========
  // BlankField
//  final case class BlankField


  // UserDefined
  // ===========
  // #PENDING
  // Loose

  implicit def univEq: UnivEq[Issue] = UnivEq.derive
}


final case class IssueId(value: Int)

final case class IssueWithId(id: IssueId, issue: Issue)

final case class Issues(list: List[Issue]) {
  lazy val count = list.size
}
