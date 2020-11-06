package shipreq.webapp.member.test.project

import shipreq.webapp.member.project.data.derivation._
import shipreq.webapp.member.project.data.{ManualIssue => ManualIssueInstance, _}
import shipreq.webapp.member.project.issue._
import shipreq.webapp.member.project.text.Text.Equality._
import shipreq.webapp.member.project.text.{Atom, Text}


sealed abstract class IssueLite(final val cls: IssueClass)
object IssueLite {
  import shipreq.webapp.member.project.issue.{IssueClass => C}

  final case class BlankCustomField(reqId: ReqId,
                                    fieldId: CustomFieldId) extends IssueLite(C.BlankCustomField)

  final case class BlankTitle(reqId: ReqId) extends IssueLite(C.BlankTitle)

  final case class BlankUseCaseStep(stepId: UseCaseStepId) extends IssueLite(C.BlankUseCaseStep)

  final case class ConflictingTags(reqId     : ReqId,
                                   tagGroupId: TagGroupId,
                                   locs      : NonEmptySet[LocationOf.Tag.InReq]) extends IssueLite(C.ConflictingTags)

  final case class DeadIssueTagInRcg(rcgId: ReqCodeGroupId,
                                     issue: Text.CodeGroupTitle.Issue) extends IssueLite(C.DeadIssueTag)

  final case class DeadIssueTagInReq(reqId: ReqId,
                                     loc  : LocationOf.Text.InReq,
                                     issue: Atom.AnyIssue) extends IssueLite(C.DeadIssueTag)

  final case class DeadRefInRcg(rcgId: ReqCodeGroupId,
                                ref  : ContentRef) extends IssueLite(C.DeadReference)

  final case class DeadRefInReq(reqId: ReqId,
                                loc  : LocationOf.Text.InReq,
                                ref  : ContentRef) extends IssueLite(C.DeadReference)

  final case class DeadTag(reqId: ReqId,
                           loc  : LocationOf.Text.InReq,
                           tagId: ApplicableTagId) extends IssueLite(C.DeadTag)

  final case class DerivativeTagResultDead(fieldId: CustomField.Tag.Id,
                                           key1   : ApplicableTagId,
                                           key2   : ApplicableTagId,
                                           tagId  : ApplicableTagId) extends IssueLite(C.DerivativeTagDead)

  final case class DerivativeTagResultUnrelated(fieldId: CustomField.Tag.Id,
                                                key1   : ApplicableTagId,
                                                key2   : ApplicableTagId,
                                                tagId  : ApplicableTagId) extends IssueLite(C.DerivativeTagUnrelated)

  final case class DuplicateTitle(reqId: ReqId) extends IssueLite(C.DuplicateTitle)

  final case class EmptyCodeGroup(rcgId: ReqCodeGroupId) extends IssueLite(C.EmptyCodeGroup)

  final case class FieldDefaultTagDead(fieldId: CustomField.Tag.Id,
                                       tagId  : ApplicableTagId) extends IssueLite(C.FieldDefaultTagDead)

  final case class FieldDefaultTagNotApplicable(fieldId  : CustomField.Tag.Id,
                                                tagId    : ApplicableTagId,
                                                reqTypeId: ReqTypeId) extends IssueLite(C.FieldDefaultTagNotApplicable)

  final case class FieldDefaultTagUnrelated(fieldId: CustomField.Tag.Id,
                                            tagId  : ApplicableTagId) extends IssueLite(C.FieldDefaultTagUnrelated)

  final case class ImplicationRequired(reqId: ReqId) extends IssueLite(C.ImplicationRequired)

  final case class IssueTagInRcg(rcgId: ReqCodeGroupId,
                                 issue: Text.CodeGroupTitle.Issue) extends IssueLite(C.IssueTag)

  final case class IssueTagInReq(reqId: ReqId,
                                 loc  : LocationOf.Text.InReq,
                                 issue: Atom.AnyIssue) extends IssueLite(C.IssueTag)

  final case class ManualIssue(issue: ManualIssueInstance) extends IssueLite(C.ManualIssue)

  final case class NonApplicableField(fieldId: CustomFieldId) extends IssueLite(C.NonApplicableField)

  final case class NonApplicableTag(reqId: ReqId,
                                    loc  : LocationOf.Text.InReq,
                                    tagId: ApplicableTagId) extends IssueLite(C.NonApplicableTag)

  final case class UninhabitableTagField(fieldId: CustomField.Tag.Id) extends IssueLite(C.UninhabitableTagField)

  implicit def univEq: UnivEq[IssueLite] = UnivEq.derive

  val fromIssue: Issue => IssueLite = {
    case Issue.BlankCustomField            (req, field           ) => BlankCustomField            (req.id, field.id)
    case Issue.BlankTitle                  (req                  ) => BlankTitle                  (req.id)
    case Issue.BlankUseCaseStep            (step                 ) => BlankUseCaseStep            (step.id)
    case Issue.ConflictingTags             (req, tagGroupId, locs) => ConflictingTags             (req.id, tagGroupId, locs)
    case Issue.DeadIssueTagInRcg           (rcg, issue           ) => DeadIssueTagInRcg           (rcg.id, issue)
    case Issue.DeadIssueTagInReq           (req, loc, issue      ) => DeadIssueTagInReq           (req.id, loc, issue)
    case Issue.DeadRefInRcg                (rcg, ref             ) => DeadRefInRcg                (rcg.id, ref)
    case Issue.DeadRefInReq                (req, loc, ref        ) => DeadRefInReq                (req.id, loc, ref)
    case Issue.DeadTag                     (req, loc, tag        ) => DeadTag                     (req.id, loc, tag.id)
    case Issue.DerivativeTagResultDead     (field, k1, k2, tag   ) => DerivativeTagResultDead     (field.id, k1.id, k2.id, tag.id)
    case Issue.DerivativeTagResultUnrelated(field, _, k1, k2, tag) => DerivativeTagResultUnrelated(field.id, k1.id, k2.id, tag.id)
    case Issue.DuplicateTitle              (req                  ) => DuplicateTitle              (req.id)
    case Issue.EmptyCodeGroup              (rcg                  ) => EmptyCodeGroup              (rcg.id)
    case Issue.FieldDefaultTagDead         (field, tag           ) => FieldDefaultTagDead         (field.id, tag.id)
    case Issue.FieldDefaultTagNotApplicable(field, tag, reqType  ) => FieldDefaultTagNotApplicable(field.id, tag.id, reqType.reqTypeId)
    case Issue.FieldDefaultTagUnrelated    (field, tag           ) => FieldDefaultTagUnrelated    (field.id, tag.id)
    case Issue.ImplicationRequired         (req                  ) => ImplicationRequired         (req.id)
    case Issue.IssueTagInRcg               (rcg, issue           ) => IssueTagInRcg               (rcg.id, issue)
    case Issue.IssueTagInReq               (req, loc, issue      ) => IssueTagInReq               (req.id, loc, issue)
    case Issue.ManualIssue                 (issue                ) => ManualIssue                 (issue)
    case Issue.NonApplicableField          (field                ) => NonApplicableField          (field.id)
    case Issue.NonApplicableTag            (req, loc, tag        ) => NonApplicableTag            (req.id, loc, tag.id)
    case Issue.UninhabitableTagField       (field                ) => UninhabitableTagField       (field.id)
  }
}
