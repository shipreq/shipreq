package shipreq.webapp.base.issue

import japgolly.microlibs.adt_macros.AdtMacros
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.data.{ManualIssue => ManualIssueInstance, _}
import shipreq.webapp.base.text.{Atom, Text}

sealed trait IssueCategory
object IssueCategory {
  case object BadData     extends IssueCategory
  case object Futility    extends IssueCategory
  case object MissingData extends IssueCategory
  case object UserDefined extends IssueCategory

  implicit def univEq: UnivEq[IssueCategory] = UnivEq.derive
  val values = AdtMacros.adtValues[IssueCategory]
}

sealed abstract class IssueClass(final val category: IssueCategory)
object IssueClass {
  import shipreq.webapp.base.issue.{IssueCategory => C}

  case object BlankCustomField             extends IssueClass(C.MissingData)
  case object BlankTitle                   extends IssueClass(C.MissingData)
  case object BlankUseCaseStep             extends IssueClass(C.MissingData)
  case object ConflictingTags              extends IssueClass(C.BadData)
  case object DeadIssueTag                 extends IssueClass(C.BadData)
  case object DeadReference                extends IssueClass(C.BadData)
  case object DeadTag                      extends IssueClass(C.BadData)
  case object DerivativeTagDead            extends IssueClass(C.BadData)
  case object DerivativeTagUnrelated       extends IssueClass(C.BadData)
  case object DuplicateTitle               extends IssueClass(C.BadData)
  case object EmptyCodeGroup               extends IssueClass(C.Futility)
  case object FieldDefaultTagDead          extends IssueClass(C.BadData)
  case object FieldDefaultTagNotApplicable extends IssueClass(C.BadData)
  case object FieldDefaultTagUnrelated     extends IssueClass(C.BadData)
  case object ImplicationRequired          extends IssueClass(C.MissingData)
  case object IssueTag                     extends IssueClass(C.UserDefined)
  case object ManualIssue                  extends IssueClass(C.UserDefined)
  case object NonApplicableField           extends IssueClass(C.Futility)
  case object NonApplicableTag             extends IssueClass(C.BadData)
  case object UninhabitableTagField        extends IssueClass(C.Futility)

  implicit def univEq: UnivEq[IssueClass] = UnivEq.derive
  val values = AdtMacros.adtValues[IssueClass]
}

sealed abstract class Issue(final val cls: IssueClass) {
  @inline def category = cls.category
}

object Issue {
  import shipreq.webapp.base.issue.{IssueClass => C}

  final case class BlankCustomField(req  : Req,
                                    field: CustomField) extends Issue(C.BlankCustomField)

  final case class BlankTitle(req: Req) extends Issue(C.BlankTitle)

  final case class BlankUseCaseStep(step: UseCaseStep.Focus) extends Issue(C.BlankUseCaseStep)

  final case class ConflictingTags(req       : Req,
                                   tagGroupId: TagGroupId,
                                   locs      : NonEmptySet[LocationOf.Tag.InReq]) extends Issue(C.ConflictingTags)

  final case class DeadIssueTagInRcg(rcg  : LiveCodeGroup,
                                     issue: Text.CodeGroupTitle.Issue) extends Issue(C.DeadIssueTag)

  final case class DeadIssueTagInReq(req  : Req,
                                     loc  : LocationOf.Text.InReq,
                                     issue: Atom.AnyIssue) extends Issue(C.DeadIssueTag)

  final case class DeadRefInRcg(rcg: LiveCodeGroup,
                                ref: ContentRef) extends Issue(C.DeadReference)

  final case class DeadRefInReq(req: Req,
                                loc: LocationOf.Text.InReq,
                                ref: ContentRef) extends Issue(C.DeadReference)

  final case class DeadTag(req: Req,
                           loc: LocationOf.Text.InReq,
                           tag: ApplicableTag) extends Issue(C.DeadTag)

  final case class DerivativeTagResultDead(field: CustomField.Tag,
                                           key1 : ApplicableTag,
                                           key2 : ApplicableTag,
                                           tag  : ApplicableTag) extends Issue(C.DerivativeTagDead)

  final case class DerivativeTagResultUnrelated(field        : CustomField.Tag,
                                                fieldTagGroup: TagGroup,
                                                key1         : ApplicableTag,
                                                key2         : ApplicableTag,
                                                tag          : ApplicableTag) extends Issue(C.DerivativeTagUnrelated)

  final case class DuplicateTitle(req: Req) extends Issue(C.DuplicateTitle)

  final case class EmptyCodeGroup(rcg: LiveCodeGroup) extends Issue(C.EmptyCodeGroup)

  final case class FieldDefaultTagDead(field: CustomField.Tag,
                                       tag  : ApplicableTag) extends Issue(C.FieldDefaultTagDead)

  final case class FieldDefaultTagNotApplicable(field  : CustomField.Tag,
                                                tag    : ApplicableTag,
                                                reqType: ReqType) extends Issue(C.FieldDefaultTagNotApplicable)

  final case class FieldDefaultTagUnrelated(field: CustomField.Tag,
                                            tag  : ApplicableTag) extends Issue(C.FieldDefaultTagUnrelated)

  final case class ImplicationRequired(req: Req) extends Issue(C.ImplicationRequired)

  final case class IssueTagInRcg(rcg  : LiveCodeGroup,
                                 issue: Text.CodeGroupTitle.Issue) extends Issue(C.IssueTag)

  final case class IssueTagInReq(req  : Req,
                                 loc  : LocationOf.Text.InReq,
                                 issue: Atom.AnyIssue) extends Issue(C.IssueTag)

  final case class ManualIssue(issue: ManualIssueInstance) extends Issue(C.ManualIssue)

  final case class NonApplicableField(field: CustomField) extends Issue(C.NonApplicableField)

  final case class NonApplicableTag(req: Req,
                                    loc: LocationOf.Text.InReq,
                                    tag: ApplicableTag) extends Issue(C.NonApplicableTag)

  final case class UninhabitableTagField(field: CustomField.Tag) extends Issue(C.UninhabitableTagField)
}

sealed trait ContentRef
object ContentRef {
  final case class ReqRef        (value: ReqId)        extends ContentRef
  final case class CodeRef       (value: ReqCodeId)    extends ContentRef
  final case class UseCaseStepRef(value: UseCaseStepId)extends ContentRef

  val fromAtom: Atom.AnyContentRef => ContentRef = {
    case a: Atom.ContentRef # ReqRef         => ReqRef        (a.id)
    case a: Atom.ContentRef # CodeRef        => CodeRef       (a.id)
    case a: Atom.ContentRef # UseCaseStepRef => UseCaseStepRef(a.value)
  }

  implicit def univEq: UnivEq[ContentRef] = UnivEq.derive
}
