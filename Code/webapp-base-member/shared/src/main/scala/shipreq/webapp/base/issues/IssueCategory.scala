package shipreq.webapp.base.issues

import shipreq.webapp.base.data._

sealed trait IssueCategory
object IssueCategory {
  case object BadData     extends IssueCategory
  case object Futility    extends IssueCategory
  case object MissingData extends IssueCategory
  case object UserDefined extends IssueCategory
}

sealed abstract class Issue(final val category: IssueCategory)
object Issue {
  import IssueCategory._

  // BadData
  // =======
  // Conflicting Priority tags
  // Deleted issue tag in use: #TBD
  // Deleted tag in use: #backend
  // Reference to deleted data

  final case class ConflictingTags(req: Req, tagGroup: TagGroup) extends Issue(BadData) {
    assert(req.liveExplicitly is Live) // TODO use props
    assert(tagGroup.mutexChildren is MutexChildren) // TODO use props
    assert(tagGroup.live is Live) // TODO use props
  }

  final case class DeadCustomIssueInUse(req: Req, issue: CustomIssueType) extends Issue(BadData)
  final case class DeadTagInUse(req: Req, tag: ApplicableTag) extends Issue(BadData)

  // Futility
  // ========
  // Code group has nothing to group.
  // Status field has no tags


  // MissingData
  // ===========
  // BlankField


  // UserDefined
  // ===========
  // #PENDING
  // Loose
}