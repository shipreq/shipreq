package shipreq.webapp.base.event

import japgolly.microlibs.nonempty._
import nyaya.util.Multimap
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable.SavedView
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.text.Text
import Text.{UseCaseStep => StepTitle, _}

/** A change to a [[Project]].
  *
  * All [[Event]]s must be readable from the DB. They must deserialise consistently.
  *
  * Only [[ActiveEvent]]s can be written to the DB.
  */
sealed trait Event

/** Events of which new instances can be created. */
sealed trait ActiveEvent extends Event

/** Events that used to be active but have been retired. They are read-only.
  *
  * The are still around because they'll have been saved to the DB and are now part of people's projects' event streams.
  */
sealed trait RetiredEvent extends Event

object Event {

  type NonEmptyCustomTextMap = NonEmpty[Map[CustomField.Text.Id, CustomTextField.NonEmptyText]]

  /*
  +===========+
  | Semantics |
  +===========+

  - these events are the low-level representation of change
    they should be fast to apply and have very little logic (if any)

  - in some cases users can't specify dead values, but events can (and should)
    typical flow is like this:

        [UI] ----> [Cmd with live values only] --------> [MakeEvent] ----> [Event with dead values included]

    which is actually preferred over

        [UI] ----> [Cmd with dead values included] ----> [MakeEvent] ----> [Event with dead values included]

    because it simplifies client/API usage, and ensures that important logic happens on our side (server) under our
    control meaning it's more dependable from a dev and system pov.


  +=============+
  | Style Guide |
  +=============+

  - Event names are in the form <noun> <verb>.
  - Create/Add: Use Create when a new id is consumed, use Add otherwise.
  - Update/Patch: Use Update when replacement values are provided in full, use Patch when diffs are provided.
  - Delete and Restore are separate events.
  - Prefer combination: Create/Delete/Restore
  - Prefer combination: Add/Remove
  */

  // ===================================================================================================================
  // Cosmetic. No impact on content (or config).

  final case class ProjectNameSet(name: String) extends ActiveEvent

  // ===================================================================================================================
  // Config: Templates

  /**
   * Currently only applies to empty projects. Application fails if there is overlap.
   */
  final case class ProjectTemplateApply(template: ProjectTemplate) extends ActiveEvent

  // ===================================================================================================================
  // Config: Custom issue types

  final case class CustomIssueTypeCreate (id: CustomIssueTypeId, vs: CustomIssueTypeGD.NonEmptyValues) extends ActiveEvent
  final case class CustomIssueTypeUpdate (id: CustomIssueTypeId, vs: CustomIssueTypeGD.NonEmptyValues) extends ActiveEvent
  final case class CustomIssueTypeDelete (id: CustomIssueTypeId)                                       extends ActiveEvent
  final case class CustomIssueTypeRestore(id: CustomIssueTypeId)                                       extends ActiveEvent

  // ===================================================================================================================
  // Config: Custom req types

  final case class CustomReqTypeCreate    (id: CustomReqTypeId, vs: CustomReqTypeGD.NonEmptyValues) extends ActiveEvent
  final case class CustomReqTypeUpdate    (id: CustomReqTypeId, vs: CustomReqTypeGD.NonEmptyValues) extends ActiveEvent
  final case class CustomReqTypeRestore   (id: CustomReqTypeId)                                     extends ActiveEvent
  final case class CustomReqTypeDeleteHard(id: CustomReqTypeId)                                     extends ActiveEvent
  final case class CustomReqTypeDeleteSoft(id: CustomReqTypeId)                                     extends ActiveEvent

  final case class CustomReqTypeDelete(id: CustomReqTypeId) extends RetiredEvent

  // ===================================================================================================================
  // Config: Tags

  // TODO Should there be a RepositionTag event?
  final case class TagDelete (id: TagId) extends ActiveEvent
  final case class TagRestore(id: TagId) extends ActiveEvent

  final case class TagGroupCreate(id: TagGroupId, vs: TagGroupGD.NonEmptyValues) extends ActiveEvent
  final case class TagGroupUpdate(id: TagGroupId, vs: TagGroupGD.NonEmptyValues) extends ActiveEvent

  final case class ApplicableTagCreate(id: ApplicableTagId, vs: ApplicableTagGD.NonEmptyValues) extends ActiveEvent
  final case class ApplicableTagUpdate(id: ApplicableTagId, vs: ApplicableTagGD.NonEmptyValues) extends ActiveEvent

  final case class ApplicableTagCreateV1(id: ApplicableTagId, vs: RetiredGenericData.ApplicableTagGDv1.NonEmptyValues) extends RetiredEvent
  final case class ApplicableTagUpdateV1(id: ApplicableTagId, vs: RetiredGenericData.ApplicableTagGDv1.NonEmptyValues) extends RetiredEvent

  // ===================================================================================================================
  // Config: Fields

  final case class FieldReposition(id: FieldId, newPos: RelPos[FieldId]) extends ActiveEvent

  final case class FieldStaticAdd   (f: StaticField) extends ActiveEvent
  final case class FieldStaticRemove(f: StaticField) extends ActiveEvent

  final case class FieldCustomDelete (id: CustomFieldId) extends ActiveEvent
  final case class FieldCustomRestore(id: CustomFieldId) extends ActiveEvent

  final case class FieldCustomTextCreate(id: CustomField.Text.Id,
                                         vs: CustomTextFieldGD.NonEmptyValues) extends ActiveEvent

  final case class FieldCustomTagCreate(id   : CustomField.Tag.Id,
                                        tagId: TagGroupId,
                                        vs   : CustomTagFieldGD.NonEmptyValues) extends ActiveEvent


  final case class FieldCustomImpCreate(id       : CustomField.Implication.Id,
                                        reqTypeId: ReqTypeId,
                                        vs       : CustomImpFieldGD.NonEmptyValues) extends ActiveEvent

  final case class FieldCustomImpUpdate (id: CustomField.Implication.Id, vs: CustomImpFieldGD .NonEmptyValues) extends ActiveEvent
  final case class FieldCustomTagUpdate (id: CustomField.Tag        .Id, vs: CustomTagFieldGD .NonEmptyValues) extends ActiveEvent
  final case class FieldCustomTextUpdate(id: CustomField.Text       .Id, vs: CustomTextFieldGD.NonEmptyValues) extends ActiveEvent

  final case class FieldCustomTextCreateV1(id: CustomField.Text       .Id, vs: RetiredGenericData.CustomTextFieldGDv1.NonEmptyValues) extends RetiredEvent
  final case class FieldCustomTextUpdateV1(id: CustomField.Text       .Id, vs: RetiredGenericData.CustomTextFieldGDv1.NonEmptyValues) extends RetiredEvent
  final case class FieldCustomTagCreateV1 (id: CustomField.Tag        .Id, vs: RetiredGenericData.CustomTagFieldGDv1 .NonEmptyValues) extends RetiredEvent
  final case class FieldCustomTagUpdateV1 (id: CustomField.Tag        .Id, vs: RetiredGenericData.CustomTagFieldGDv1 .NonEmptyValues) extends RetiredEvent
  final case class FieldCustomImpCreateV1 (id: CustomField.Implication.Id, vs: RetiredGenericData.CustomImpFieldGDv1 .NonEmptyValues) extends RetiredEvent
  final case class FieldCustomImpUpdateV1 (id: CustomField.Implication.Id, vs: RetiredGenericData.CustomImpFieldGDv1 .NonEmptyValues) extends RetiredEvent

  // ===================================================================================================================
  // Content: Generic requirements

  final case class GenericReqCreate  (id: GenericReqId, rt: CustomReqTypeId, vs: GenericReqGD.Values) extends ActiveEvent
  final case class GenericReqTypeSet (id: GenericReqId, value: CustomReqTypeId)                       extends ActiveEvent
  final case class GenericReqTitleSet(id: GenericReqId, value: GenericReqTitle.OptionalText)          extends ActiveEvent

  // ===================================================================================================================
  // Content: Use cases

  /**
   * @param stepId Use cases have a mandatory root step. This guarantees the determinism of the root step ID.
   */
  final case class UseCaseCreate(id: UseCaseId, stepId: UseCaseStepId, vs: UseCaseGD.Values) extends ActiveEvent

  final case class UseCaseTitleSet(id: UseCaseId, value: UseCaseTitle.OptionalText) extends ActiveEvent

  final case class UseCaseStepCreate(id   : UseCaseStepId,
                                     ucId : UseCaseId,
                                     field: StaticField.UseCaseStepTree,
                                     at   : VectorTree.ParentLocation) extends ActiveEvent

  final case class UseCaseStepUpdate    (id: UseCaseStepId, vs: UseCaseStepGD.NonEmptyValues) extends ActiveEvent
  final case class UseCaseStepShiftLeft (id: UseCaseStepId)                                   extends ActiveEvent
  final case class UseCaseStepShiftRight(id: UseCaseStepId)                                   extends ActiveEvent
  final case class UseCaseStepDelete    (id: UseCaseStepId)                                   extends ActiveEvent
  final case class UseCaseStepRestore   (id: UseCaseStepId)                                   extends ActiveEvent

  // ===================================================================================================================
  // Content: ReqCode groups

  final case class CodeGroupCreate(id: ReqCodeGroupId, vs: CodeGroupGD.NonEmptyValues) extends ActiveEvent
  final case class CodeGroupUpdate(id: ReqCodeGroupId, vs: CodeGroupGD.NonEmptyValues) extends ActiveEvent
  final case class CodeGroupsDelete(ids: NonEmptySet[ReqCodeGroupId]) extends ActiveEvent

  // ===================================================================================================================
  // Content: Shared

  /**
   * Updates a requirement's reqcodes.
   *
   * When a reqcode is renamed it appears both in `remove` and `add`.
   * Eg. `remove=3, add=(new.name: 3)`.
   *
   * @param remove Codes to remove. Those referenced in text will be soft-deleted.
   * @param restore Soft-deleted codes to restore back to active status.
   * @param add Codes to add. A code can have multiple IDs (see [[ApplyEvent.ReqCodeLogic]] for details) in which case,
   *            only one becomes active and the rest go into `reqInactive`.
   */
  final case class ReqCodesPatch(id     : ReqId,
                                 remove : Set[ApReqCodeId],
                                 restore: Set[ApReqCodeId],
                                 add    : Multimap[ReqCode.Value, Set, ApReqCodeId]) extends ActiveEvent

  final case class ReqTagsPatch(id   : ReqId,
                                patch: SetDiff.NE[ApplicableTagId]) extends ActiveEvent

  final case class ReqImplicationsPatch(id   : ReqId,
                                        dir  : Direction,
                                        patch: SetDiff.NE[ReqId]) extends ActiveEvent

  final case class ReqFieldCustomTextSet(id   : ReqId,
                                         fid  : CustomField.Text.Id,
                                         value: CustomTextField.OptionalText) extends ActiveEvent

  final case class ReqsDelete(reqs      : NonEmptySet[ReqId],
                              codeGroups: Set[ReqCodeGroupId],
                              reason    : DeletionReason.OptionalText) extends ActiveEvent

  final case class ContentRestore(reqs      : Set[ReqId],
                                  codeGroups: Set[ReqCodeGroupId]) extends ActiveEvent

  // ===================================================================================================================
  // Manual issues

  final case class ManualIssueCreate(id  : ManualIssueId,
                                     text: Text.ManualIssue.NonEmptyText) extends ActiveEvent

  final case class ManualIssueUpdate(id  : ManualIssueId,
                                     text: Text.ManualIssue.NonEmptyText) extends ActiveEvent

  final case class ManualIssueDelete(id: ManualIssueId) extends ActiveEvent

  // ===================================================================================================================
  // Saved Views

  final case class SavedViewCreate(id        : SavedView.Id,
                                   name      : SavedView.Name,
                                   columns   : NonEmptyVector[reqtable.Column],
                                   order     : reqtable.SortCriteria,
                                   filterDead: FilterDead,
                                   filter    : Option[Filter.Valid]) extends ActiveEvent

  final case class SavedViewUpdate    (id: SavedView.Id, vs: SavedViewGD.NonEmptyValues) extends ActiveEvent
  final case class SavedViewDelete    (id: SavedView.Id)                                 extends ActiveEvent
  final case class SavedViewDefaultSet(id: SavedView.Id)                                 extends ActiveEvent
}