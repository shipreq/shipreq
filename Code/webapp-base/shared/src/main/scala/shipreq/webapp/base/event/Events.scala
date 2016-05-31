package shipreq.webapp.base.event

import nyaya.util.Multimap
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.util._
import Text.{UseCaseStep => StepTitle, _}
import Text.Equality._

/*
===============
= Style Guide =
===============
Event names are in the form <noun> <verb>.
Create/Add: Use Create when a new id is consumed, use Add otherwise.
Update/Patch: Use Update when replacement values are provided in full, use Patch when diffs are provided.
Delete and Restore are separate events.
Prefer combination: Create/Delete/Restore
Prefer combination: Add/Remove
*/

/**
 * A change to a [[Project]].
 *
 * All [[Event]]s must be readable from the DB. They must deserialise consistently.
 *
 * Only [[ActiveEvent]]s can be written to the DB.
 */
sealed trait Event

/**
 * Events of which new instances can be created.
 *
 * (Events can be retired over time.)
 */
sealed trait ActiveEvent extends Event

// =====================================================================================================================
// Cosmetic. No impact on content (or config).

final case class ProjectNameSet(name: String) extends ActiveEvent

// =====================================================================================================================
// Config: Templates

/**
 * Currently only applies to empty projects. Application fails if there is overlap.
 */
final case class ProjectTemplateApply(template: ProjectTemplate) extends ActiveEvent

// =====================================================================================================================
// Config: Custom issue types

@CreateGenericData
object CustomIssueTypeGD extends GenericData {
  val Key  = defAttr[HashRefKey]
  val Desc = defAttr[Option[String]]
}

final case class CustomIssueTypeCreate (id: CustomIssueTypeId, vs: CustomIssueTypeGD.NonEmptyValues) extends ActiveEvent
final case class CustomIssueTypeUpdate (id: CustomIssueTypeId, vs: CustomIssueTypeGD.NonEmptyValues) extends ActiveEvent
final case class CustomIssueTypeDelete (id: CustomIssueTypeId)                                       extends ActiveEvent
final case class CustomIssueTypeRestore(id: CustomIssueTypeId)                                       extends ActiveEvent

// =====================================================================================================================
// Config: Custom req types

@CreateGenericData
object CustomReqTypeGD extends GenericData {
  val Mnemonic = defAttr[ReqType.Mnemonic]
  val Name     = defAttr[String]
  val Imp      = defAttr[ImplicationRequired]
}

final case class CustomReqTypeCreate (id: CustomReqTypeId, vs: CustomReqTypeGD.NonEmptyValues) extends ActiveEvent
final case class CustomReqTypeUpdate (id: CustomReqTypeId, vs: CustomReqTypeGD.NonEmptyValues) extends ActiveEvent
final case class CustomReqTypeDelete (id: CustomReqTypeId)                                     extends ActiveEvent
final case class CustomReqTypeRestore(id: CustomReqTypeId)                                     extends ActiveEvent

// =====================================================================================================================
// Config: Tags

// TODO Should there be a RepositionTag event?
final case class TagDelete (id: TagId) extends ActiveEvent
final case class TagRestore(id: TagId) extends ActiveEvent

@CreateGenericData
object TagGroupGD extends GenericData {
  val Name          = defAttr[String]
  val Desc          = defAttr[Option[String]]
  val MutexChildren = defAttr[MutexChildren]
  val Parents       = defAttr[TagInTree.Parents]
  val Children      = defAttr[TagInTree.Children]
}

final case class TagGroupCreate(id: TagGroupId, vs: TagGroupGD.NonEmptyValues) extends ActiveEvent
final case class TagGroupUpdate(id: TagGroupId, vs: TagGroupGD.NonEmptyValues) extends ActiveEvent

@CreateGenericData
object ApplicableTagGD extends GenericData {
  val Name     = defAttr[String]
  val Desc     = defAttr[Option[String]]
  val Key      = defAttr[HashRefKey]
  val Parents  = defAttr[TagInTree.Parents]
  val Children = defAttr[TagInTree.Children]
}

final case class ApplicableTagCreate(id: ApplicableTagId, vs: ApplicableTagGD.NonEmptyValues) extends ActiveEvent
final case class ApplicableTagUpdate(id: ApplicableTagId, vs: ApplicableTagGD.NonEmptyValues) extends ActiveEvent

// =====================================================================================================================
// Config: Fields

final case class FieldReposition(id: FieldId, newPos: RelPos[FieldId]) extends ActiveEvent

final case class FieldStaticAdd   (f: StaticField) extends ActiveEvent
final case class FieldStaticRemove(f: StaticField) extends ActiveEvent

final case class FieldCustomDelete (id: CustomFieldId) extends ActiveEvent
final case class FieldCustomRestore(id: CustomFieldId) extends ActiveEvent

@CreateGenericData
object CustomTextFieldGD extends GenericData {
  val Name      = defAttr[String]
  val Key       = defAttr[FieldRefKey]
  val Mandatory = defAttr[Mandatory]
  val ReqTypes  = defAttr[Field.ApplicableReqTypes]
}
final case class FieldCustomTextCreate(id: CustomField.Text.Id, vs: CustomTextFieldGD.NonEmptyValues) extends ActiveEvent
final case class FieldCustomTextUpdate(id: CustomField.Text.Id, vs: CustomTextFieldGD.NonEmptyValues) extends ActiveEvent

@CreateGenericData
object CustomTagFieldGD extends GenericData {
  val TagId     = defAttr[TagId]
  val Mandatory = defAttr[Mandatory]
  val ReqTypes  = defAttr[Field.ApplicableReqTypes]
}
final case class FieldCustomTagCreate(id: CustomField.Tag.Id, vs: CustomTagFieldGD.NonEmptyValues) extends ActiveEvent
final case class FieldCustomTagUpdate(id: CustomField.Tag.Id, vs: CustomTagFieldGD.NonEmptyValues) extends ActiveEvent

@CreateGenericData
object CustomImpFieldGD extends GenericData {
  val ReqTypeId = defAttr[ReqTypeId]
  val Mandatory = defAttr[Mandatory]
  val ReqTypes  = defAttr[Field.ApplicableReqTypes]
}
final case class FieldCustomImpCreate(id: CustomField.Implication.Id, vs: CustomImpFieldGD.NonEmptyValues) extends ActiveEvent
final case class FieldCustomImpUpdate(id: CustomField.Implication.Id, vs: CustomImpFieldGD.NonEmptyValues) extends ActiveEvent

// =====================================================================================================================
// Content: Generic requirements

/**
 * This is only used in [[GenericReqCreate]].
 * All fields are optional; any mandatory data is required in the [[GenericReqCreate]] constructor directly. Thus,
 * when a field is provided, its value must be non-empty (with emptiness representable by omitting the field).
 */
@CreateGenericData
object GenericReqGD extends GenericData {
  val Title    = defAttr[GenericReqTitle.NonEmptyText]
  val ReqCodes = defAttr[NonEmptySet[ReqCode.IdAndValue]]
  val Tags     = defAttr[NonEmptySet[ApplicableTagId]]
  val ImpSrcs  = defAttr[NonEmptySet[ReqId]]
  val ImpTgts  = defAttr[NonEmptySet[ReqId]]
}

final case class GenericReqCreate  (id: GenericReqId, rt: CustomReqTypeId, vs: GenericReqGD.Values) extends ActiveEvent
final case class GenericReqTypeSet (id: GenericReqId, value: CustomReqTypeId)                       extends ActiveEvent
final case class GenericReqTitleSet(id: GenericReqId, value: GenericReqTitle.OptionalText)          extends ActiveEvent

// =====================================================================================================================
// Content: Use cases

/**
 * This is only used in [[UseCaseCreate]].
 * All fields are optional; any mandatory data is required in the [[UseCaseCreate]] constructor directly. Thus,
 * when a field is provided, its value must be non-empty (with emptiness representable by omitting the field).
 */
@CreateGenericData
object UseCaseGD extends GenericData {
  val Title    = defAttr[UseCaseTitle.NonEmptyText]
  val ReqCodes = defAttr[NonEmptySet[ReqCode.IdAndValue]]
  val Tags     = defAttr[NonEmptySet[ApplicableTagId]]
  val ImpSrcs  = defAttr[NonEmptySet[ReqId]]
  val ImpTgts  = defAttr[NonEmptySet[ReqId]]
}

/**
 * @param stepId Use cases have a mandatory root step. This guarantees the determinism of the root step ID.
 */
final case class UseCaseCreate(id: UseCaseId, stepId: UseCaseStepId, vs: UseCaseGD.Values) extends ActiveEvent

final case class UseCaseTitleSet(id: UseCaseId, value: UseCaseTitle.OptionalText) extends ActiveEvent

@CreateGenericData
object UseCaseStepGD extends GenericData {
  val Title   = defAttr[StepTitle.OptionalText]
  val FlowIn  = defAttr[SetDiff.NE[UseCaseStepId]]
  val FlowOut = defAttr[SetDiff.NE[UseCaseStepId]]
}

final case class UseCaseStepCreate(id   : UseCaseStepId,
                                   ucId : UseCaseId,
                                   field: StaticField.UseCaseStepTree,
                                   at   : VectorTree.ParentLocation) extends ActiveEvent

final case class UseCaseStepUpdate    (id: UseCaseStepId, vs: UseCaseStepGD.NonEmptyValues) extends ActiveEvent
final case class UseCaseStepShiftLeft (id: UseCaseStepId)                                   extends ActiveEvent
final case class UseCaseStepShiftRight(id: UseCaseStepId)                                   extends ActiveEvent
final case class UseCaseStepDelete    (id: UseCaseStepId)                                   extends ActiveEvent
final case class UseCaseStepRestore   (id: UseCaseStepId)                                   extends ActiveEvent

// =====================================================================================================================
// Content: ReqCode groups

@CreateGenericData
object ReqCodeGroupGD extends GenericData {
  val Code  = defAttr[ReqCode.Value]
  val Title = defAttr[ReqCodeGroupTitle.OptionalText]
}

final case class ReqCodeGroupCreate(id: ReqCodeId, vs: ReqCodeGroupGD.NonEmptyValues) extends ActiveEvent
final case class ReqCodeGroupUpdate(id: ReqCodeId, vs: ReqCodeGroupGD.NonEmptyValues) extends ActiveEvent

final case class ReqCodeGroupsDelete(ids: NonEmptySet[ReqCodeId]) extends ActiveEvent

// =====================================================================================================================
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
                               remove : Set[ReqCodeId],
                               restore: Set[ReqCodeId],
                               add    : Multimap[ReqCode.Value, Set, ReqCodeId]) extends ActiveEvent

final case class ReqTagsPatch(id   : ReqId,
                              patch: SetDiff.NE[ApplicableTagId]) extends ActiveEvent

final case class ReqImplicationsPatch(id   : ReqId,
                                      dir  : Direction,
                                      patch: SetDiff.NE[ReqId]) extends ActiveEvent

final case class ReqFieldCustomTextSet(id   : ReqId,
                                       fid  : CustomField.Text.Id,
                                       value: CustomTextField.OptionalText) extends ActiveEvent

final case class ReqsDelete(reqs         : NonEmptySet[ReqId],
                            reqCodeGroups: Set[ReqCodeId],
                            reason       : DeletionReason.OptionalText) extends ActiveEvent

// TODO Would it be better to have a ReqCodeGroupId which is a subtype of ReqCodeId?
final case class ContentRestore(reqs         : Set[ReqId],
                                reqCodeGroups: Set[ReqCodeId]) extends ActiveEvent
