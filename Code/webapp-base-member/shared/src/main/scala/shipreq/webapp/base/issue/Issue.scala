package shipreq.webapp.base.issue

import japgolly.microlibs.nonempty.NonEmptySet
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

sealed abstract class IssueClass(final val category: IssueCategory)
object IssueClass {
  import shipreq.webapp.base.issue.{IssueCategory => C}

  case object ConflictingTags       extends IssueClass(C.BadData)
  case object DeadIssueTag          extends IssueClass(C.BadData)
  case object DeadTag               extends IssueClass(C.BadData)
  case object DeadReference         extends IssueClass(C.BadData)
  case object EmptyCodeGroup        extends IssueClass(C.Futility)
  case object UninhabitableTagField extends IssueClass(C.Futility)
  case object EmptyField            extends IssueClass(C.MissingData)
  case object IssueTag              extends IssueClass(C.UserDefined)
  case object LooseIssue            extends IssueClass(C.UserDefined)

  implicit def univEq: UnivEq[IssueClass] = UnivEq.derive
}

sealed abstract class Issue(final val cls: IssueClass)
object Issue {
  import shipreq.webapp.base.issue.{IssueClass => C}

  final case class ConflictingTags(reqId     : ReqId,
                                   tagGroupId: TagGroupId,
                                   locs      : NonEmptySet[ReqTagLoc]) extends Issue(C.ConflictingTags)

  final case class EmptyCodeGroup(code: ReqCode.Value) extends Issue(C.EmptyCodeGroup)

  final case class UninhabitableTagField(fieldId: CustomField.Tag.Id) extends Issue(C.UninhabitableTagField)

  implicit def univEq: UnivEq[Issue] = UnivEq.derive
}
