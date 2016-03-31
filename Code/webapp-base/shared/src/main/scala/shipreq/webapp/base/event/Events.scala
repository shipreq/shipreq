package shipreq.webapp.base.event

import nyaya.util.Multimap
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.util._
import Text.{UseCaseStep => StepTitle, _}
import Text.Equality._
import UnivEq.Implicits._

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
// Config: Templates

/**
 * Currently only applies to empty projects. Application fails if there is overlap.
 */
case class ApplyTemplate(t: ProjectTemplate) extends ActiveEvent

// =====================================================================================================================
// Config: Custom issue types

@CreateGenericData
object CustomIssueTypeGD extends GenericData {
  val Key  = defAttr[HashRefKey]
  val Desc = defAttr[Option[String]]
}

case class CreateCustomIssueType(id: CustomIssueTypeId, vs: CustomIssueTypeGD.NonEmptyValues) extends ActiveEvent
case class UpdateCustomIssueType(id: CustomIssueTypeId, vs: CustomIssueTypeGD.NonEmptyValues) extends ActiveEvent
case class DeleteCustomIssueType(id: CustomIssueTypeId, da: DeletionAction)                   extends ActiveEvent

// =====================================================================================================================
// Config: Custom req types

@CreateGenericData
object CustomReqTypeGD extends GenericData {
  val Mnemonic = defAttr[ReqType.Mnemonic]
  val Name     = defAttr[String]
  val Imp      = defAttr[ImplicationRequired]
}

case class CreateCustomReqType(id: CustomReqTypeId, vs: CustomReqTypeGD.NonEmptyValues) extends ActiveEvent
case class UpdateCustomReqType(id: CustomReqTypeId, vs: CustomReqTypeGD.NonEmptyValues) extends ActiveEvent
case class DeleteCustomReqType(id: CustomReqTypeId, da: DeletionAction)                 extends ActiveEvent

// =====================================================================================================================
// Config: Tags

// TODO Should there be a RepositionTag event?
case class DeleteTag(id: TagId, da: DeletionAction) extends ActiveEvent

@CreateGenericData
object TagGroupGD extends GenericData {
  val Name          = defAttr[String]
  val Desc          = defAttr[Option[String]]
  val MutexChildren = defAttr[MutexChildren]
  val Parents       = defAttr[TagInTree.Parents]
  val Children      = defAttr[TagInTree.Children]
}

case class CreateTagGroup(id: TagGroupId, vs: TagGroupGD.NonEmptyValues) extends ActiveEvent
case class UpdateTagGroup(id: TagGroupId, vs: TagGroupGD.NonEmptyValues) extends ActiveEvent

@CreateGenericData
object ApplicableTagGD extends GenericData {
  val Name     = defAttr[String]
  val Desc     = defAttr[Option[String]]
  val Key      = defAttr[HashRefKey]
  val Parents  = defAttr[TagInTree.Parents]
  val Children = defAttr[TagInTree.Children]
}

case class CreateApplicableTag(id: ApplicableTagId, vs: ApplicableTagGD.NonEmptyValues) extends ActiveEvent
case class UpdateApplicableTag(id: ApplicableTagId, vs: ApplicableTagGD.NonEmptyValues) extends ActiveEvent

// =====================================================================================================================
// Config: Fields

case class RepositionField(id: FieldId, newPos: RelPos[FieldId]) extends ActiveEvent

case class AddStaticField(f: StaticField) extends ActiveEvent
case class DeleteStaticField(f: StaticField) extends ActiveEvent

case class DeleteCustomField(id: CustomFieldId, da: DeletionAction) extends ActiveEvent

@CreateGenericData
object CustomTextFieldGD extends GenericData {
  val Name      = defAttr[String]
  val Key       = defAttr[FieldRefKey]
  val Mandatory = defAttr[Mandatory]
  val ReqTypes  = defAttr[Field.ApplicableReqTypes]
}
case class CreateCustomTextField(id: CustomField.Text.Id, vs: CustomTextFieldGD.NonEmptyValues) extends ActiveEvent
case class UpdateCustomTextField(id: CustomField.Text.Id, vs: CustomTextFieldGD.NonEmptyValues) extends ActiveEvent

@CreateGenericData
object CustomTagFieldGD extends GenericData {
  val TagId     = defAttr[TagId]
  val Mandatory = defAttr[Mandatory]
  val ReqTypes  = defAttr[Field.ApplicableReqTypes]
}
case class CreateCustomTagField(id: CustomField.Tag.Id, vs: CustomTagFieldGD.NonEmptyValues) extends ActiveEvent
case class UpdateCustomTagField(id: CustomField.Tag.Id, vs: CustomTagFieldGD.NonEmptyValues) extends ActiveEvent

@CreateGenericData
object CustomImpFieldGD extends GenericData {
  val ReqTypeId = defAttr[ReqTypeId]
  val Mandatory = defAttr[Mandatory]
  val ReqTypes  = defAttr[Field.ApplicableReqTypes]
}
case class CreateCustomImpField(id: CustomField.Implication.Id, vs: CustomImpFieldGD.NonEmptyValues) extends ActiveEvent
case class UpdateCustomImpField(id: CustomField.Implication.Id, vs: CustomImpFieldGD.NonEmptyValues) extends ActiveEvent

// =====================================================================================================================
// Content: Generic requirements

/**
 * This is only used in [[CreateGenericReq]].
 * All fields are optional; any mandatory data is required in the [[CreateGenericReq]] constructor directly. Thus,
 * when a field is provided, its value must be non-empty (with emptiness representable by omitting the field).
 */
@CreateGenericData
object CreateGenericReqGD extends GenericData {
  val Title    = defAttr[GenericReqTitle.NonEmptyText]
  val ReqCodes = defAttr[NonEmptySet[ReqCode.IdAndValue]]
  val Tags     = defAttr[NonEmptySet[ApplicableTagId]]
  val ImpSrcs  = defAttr[NonEmptySet[ReqId]]
  val ImpTgts  = defAttr[NonEmptySet[ReqId]]
}

case class CreateGenericReq  (id: GenericReqId, rt: CustomReqTypeId, vs: CreateGenericReqGD.Values) extends ActiveEvent
case class SetGenericReqType (id: GenericReqId, value: CustomReqTypeId)                             extends ActiveEvent
case class SetGenericReqTitle(id: GenericReqId, value: GenericReqTitle.OptionalText)                extends ActiveEvent

// =====================================================================================================================
// Content: Use cases

/**
 * This is only used in [[CreateUseCase]].
 * All fields are optional; any mandatory data is required in the [[CreateUseCase]] constructor directly. Thus,
 * when a field is provided, its value must be non-empty (with emptiness representable by omitting the field).
 */
@CreateGenericData
object CreateUseCaseGD extends GenericData {
  val Title    = defAttr[UseCaseTitle.NonEmptyText]
  val ReqCodes = defAttr[NonEmptySet[ReqCode.IdAndValue]]
  val Tags     = defAttr[NonEmptySet[ApplicableTagId]]
  val ImpSrcs  = defAttr[NonEmptySet[ReqId]]
  val ImpTgts  = defAttr[NonEmptySet[ReqId]]
}

/**
 * @param stepId Use cases have a mandatory root step. This guarantees the determinism of the root step ID.
 */
case class CreateUseCase(id: UseCaseId, stepId: UseCaseStepId, vs: CreateUseCaseGD.Values) extends ActiveEvent

case class SetUseCaseTitle(id: UseCaseId, value: UseCaseTitle.OptionalText) extends ActiveEvent

@CreateGenericData
object UseCaseStepGD extends GenericData {
  val Title   = defAttr[StepTitle.OptionalText]
  val FlowIn  = defAttr[SetDiff.NE[UseCaseStepId]]
  val FlowOut = defAttr[SetDiff.NE[UseCaseStepId]]
}

case class AddUseCaseStep(id   : UseCaseStepId,
                          ucId : UseCaseId,
                          field: StaticField.UseCaseStepTree,
                          at   : VectorTree.ParentLocation) extends ActiveEvent

case class UpdateUseCaseStep    (id: UseCaseStepId, vs: UseCaseStepGD.NonEmptyValues) extends ActiveEvent
case class ShiftUseCaseStepLeft (id: UseCaseStepId)                                   extends ActiveEvent
case class ShiftUseCaseStepRight(id: UseCaseStepId)                                   extends ActiveEvent
case class DeleteUseCaseStep    (id: UseCaseStepId)                                   extends ActiveEvent

// =====================================================================================================================
// Content: ReqCode groups

@CreateGenericData
object ReqCodeGroupGD extends GenericData {
  val Code  = defAttr[ReqCode.Value]
  val Title = defAttr[ReqCodeGroupTitle.OptionalText]
}

case class CreateReqCodeGroup(id: ReqCodeId, vs: ReqCodeGroupGD.NonEmptyValues) extends ActiveEvent
case class UpdateReqCodeGroup(id: ReqCodeId, vs: ReqCodeGroupGD.NonEmptyValues) extends ActiveEvent

case class DeleteReqCodeGroups(ids: NonEmptySet[ReqCodeId]) extends ActiveEvent

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
case class PatchReqCodes(id     : ReqId,
                         remove : Set[ReqCodeId],
                         restore: Set[ReqCodeId],
                         add    : Multimap[ReqCode.Value, Set, ReqCodeId]) extends ActiveEvent

case class PatchReqTags        (id: ReqId, patch: SetDiff.NE[ApplicableTagId]) extends ActiveEvent
case class PatchImplicationSrc (id: ReqId, patch: SetDiff.NE[ReqId])           extends ActiveEvent
case class PatchImplicationTgt (id: ReqId, patch: SetDiff.NE[ReqId])           extends ActiveEvent
// TODO Better for PatchImplication to take a Direction?

case class SetCustomTextField(id: ReqId, fid: CustomField.Text.Id, value: CustomTextField.OptionalText) extends ActiveEvent

case class DeleteReqs(reqs: NonEmptySet[ReqId], reqCodeGroups: Set[ReqCodeId], reason: DeletionReason.OptionalText) extends ActiveEvent

// TODO Would it be better to have a ReqCodeGroupId which is a subtype of ReqCodeId?
case class RestoreContent(reqs: Set[ReqId], reqCodeGroups: Set[ReqCodeId]) extends ActiveEvent
