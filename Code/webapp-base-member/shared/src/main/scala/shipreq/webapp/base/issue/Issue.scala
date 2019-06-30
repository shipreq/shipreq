package shipreq.webapp.base.issue

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.univeq.UnivEq
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Atom, Text}
import shipreq.webapp.base.text.Text.Equality._

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

  case object BlankCustomField      extends IssueClass(C.MissingData)
  case object BlankTitle            extends IssueClass(C.MissingData)
  case object BlankUseCaseStep      extends IssueClass(C.MissingData)
  case object ConflictingTags       extends IssueClass(C.BadData)
  case object DeadIssueTag          extends IssueClass(C.BadData)
  case object DeadReference         extends IssueClass(C.BadData)
  case object DeadTag               extends IssueClass(C.BadData)
  case object EmptyCodeGroup        extends IssueClass(C.Futility)
  case object ImplicationRequired   extends IssueClass(C.MissingData)
  case object IssueTag              extends IssueClass(C.UserDefined)
  case object LooseIssue            extends IssueClass(C.UserDefined)
  case object UninhabitableTagField extends IssueClass(C.Futility)

  implicit def univEq: UnivEq[IssueClass] = UnivEq.derive
}

sealed abstract class Issue(final val cls: IssueClass)
object Issue {
  import shipreq.webapp.base.issue.{IssueClass => C}

  final case class BlankCustomField(reqId: ReqId,
                                    fieldId: CustomFieldId) extends Issue(C.BlankCustomField)

  final case class BlankTitle(reqId: ReqId) extends Issue(C.BlankTitle)

  final case class BlankUseCaseStep(stepId: UseCaseStepId) extends Issue(C.BlankUseCaseStep)

  final case class ConflictingTags(reqId     : ReqId,
                                   tagGroupId: TagGroupId,
                                   locs      : NonEmptySet[ReqTagLoc]) extends Issue(C.ConflictingTags)

  final case class DeadIssueTagInRcg(rcgId: ReqCodeGroupId,
                                     issue: Text.CodeGroupTitle.Issue) extends Issue(C.DeadIssueTag)

  final case class DeadIssueTagInReq(reqId: ReqId,
                                     loc  : ReqTextLoc,
                                     issue: Atom.AnyIssue) extends Issue(C.DeadIssueTag)

  final case class DeadRefInRcg(rcgId: ReqCodeGroupId,
                                ref  : ContentRef) extends Issue(C.DeadReference)

  final case class DeadRefInReq(reqId: ReqId,
                                loc  : ReqTextLoc,
                                ref  : ContentRef) extends Issue(C.DeadReference)

  final case class DeadTag(reqId: ReqId,
                           loc  : ReqTextLoc,
                           tagId: ApplicableTagId) extends Issue(C.DeadIssueTag)

  final case class EmptyCodeGroup(rcgId: ReqCodeGroupId) extends Issue(C.EmptyCodeGroup)

  final case class IssueTagInRcg(rcgId: ReqCodeGroupId,
                                 issue: Text.CodeGroupTitle.Issue) extends Issue(C.IssueTag)

  final case class IssueTagInReq(reqId: ReqId,
                                 loc  : ReqTextLoc,
                                 issue: Atom.AnyIssue) extends Issue(C.IssueTag)

  final case class UninhabitableTagField(fieldId: CustomField.Tag.Id) extends Issue(C.UninhabitableTagField)

  implicit def univEq: UnivEq[Issue] = UnivEq.derive
}

sealed trait ContentRef
object ContentRef {
  final case class ReqRef        (value: ReqId)        extends ContentRef
  final case class CodeRef       (value: ReqCodeId)    extends ContentRef
  final case class UseCaseStepRef(value: UseCaseStepId)extends ContentRef

  val fromAtom: Atom.AnyContentRef => ContentRef = {
    case a: Atom.ContentRef # ReqRef         => ReqRef        (a.value)
    case a: Atom.ContentRef # CodeRef        => CodeRef       (a.value)
    case a: Atom.ContentRef # UseCaseStepRef => UseCaseStepRef(a.value)
  }

  implicit def univEq: UnivEq[ContentRef] = UnivEq.derive
}
