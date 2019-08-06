package shipreq.webapp.base.test

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.univeq.UnivEq
import shipreq.webapp.base.data.{ManualIssue => ManualIssueInstance, _}
import shipreq.webapp.base.issue._
import shipreq.webapp.base.text.{Atom, Text}
import shipreq.webapp.base.text.Text.Equality._

sealed abstract class IssueLite(final val cls: IssueClass)
object IssueLite {
  import shipreq.webapp.base.issue.{IssueClass => C}

  final case class BlankCustomField(reqId: ReqId,
                                    fieldId: CustomFieldId) extends IssueLite(C.BlankCustomField)

  final case class BlankTitle(reqId: ReqId) extends IssueLite(C.BlankTitle)

  final case class BlankUseCaseStep(stepId: UseCaseStepId) extends IssueLite(C.BlankUseCaseStep)

  final case class ConflictingTags(reqId     : ReqId,
                                   tagGroupId: TagGroupId,
                                   locs      : NonEmptySet[ReqTagLoc]) extends IssueLite(C.ConflictingTags)

  final case class DeadIssueTagInRcg(rcgId: ReqCodeGroupId,
                                     issue: Text.CodeGroupTitle.Issue) extends IssueLite(C.DeadIssueTag)

  final case class DeadIssueTagInReq(reqId: ReqId,
                                     loc  : ReqTextLoc,
                                     issue: Atom.AnyIssue) extends IssueLite(C.DeadIssueTag)

  final case class DeadRefInRcg(rcgId: ReqCodeGroupId,
                                ref  : ContentRef) extends IssueLite(C.DeadReference)

  final case class DeadRefInReq(reqId: ReqId,
                                loc  : ReqTextLoc,
                                ref  : ContentRef) extends IssueLite(C.DeadReference)

  final case class DeadTag(reqId: ReqId,
                           loc  : ReqTextLoc,
                           tagId: ApplicableTagId) extends IssueLite(C.DeadIssueTag)

  final case class EmptyCodeGroup(rcgId: ReqCodeGroupId) extends IssueLite(C.EmptyCodeGroup)

  final case class ImplicationRequired(reqId: ReqId) extends IssueLite(C.ImplicationRequired)

  final case class IssueTagInRcg(rcgId: ReqCodeGroupId,
                                 issue: Text.CodeGroupTitle.Issue) extends IssueLite(C.IssueTag)

  final case class IssueTagInReq(reqId: ReqId,
                                 loc  : ReqTextLoc,
                                 issue: Atom.AnyIssue) extends IssueLite(C.IssueTag)

  final case class UninhabitableTagField(fieldId: CustomField.Tag.Id) extends IssueLite(C.UninhabitableTagField)

  final case class ManualIssue(issue: ManualIssueInstance) extends IssueLite(C.ManualIssue)

  implicit def univEq: UnivEq[IssueLite] = UnivEq.derive

  val fromIssue: Issue => IssueLite = {
    case Issue.BlankCustomField     (req, field           ) => BlankCustomField     (req.id, field.id)
    case Issue.BlankTitle           (req                  ) => BlankTitle           (req.id)
    case Issue.BlankUseCaseStep     (step                 ) => BlankUseCaseStep     (step.id)
    case Issue.ConflictingTags      (req, tagGroupId, locs) => ConflictingTags      (req.id, tagGroupId, locs)
    case Issue.DeadIssueTagInRcg    (rcg, issue           ) => DeadIssueTagInRcg    (rcg.id, issue)
    case Issue.DeadIssueTagInReq    (req, loc, issue      ) => DeadIssueTagInReq    (req.id, loc, issue)
    case Issue.DeadRefInRcg         (rcg, ref             ) => DeadRefInRcg         (rcg.id, ref)
    case Issue.DeadRefInReq         (req, loc, ref        ) => DeadRefInReq         (req.id, loc, ref)
    case Issue.DeadTag              (req, loc, tag        ) => DeadTag              (req.id, loc, tag.id)
    case Issue.EmptyCodeGroup       (rcg                  ) => EmptyCodeGroup       (rcg.id)
    case Issue.ImplicationRequired  (req                  ) => ImplicationRequired  (req.id)
    case Issue.IssueTagInRcg        (rcg, issue           ) => IssueTagInRcg        (rcg.id, issue)
    case Issue.IssueTagInReq        (req, loc, issue      ) => IssueTagInReq        (req.id, loc, issue)
    case Issue.ManualIssue          (issue                ) => ManualIssue          (issue)
    case Issue.UninhabitableTagField(field                ) => UninhabitableTagField(field.id)
  }
}
