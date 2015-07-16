package shipreq.webapp.base.event

import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text.GenericReqTitle
import shipreq.webapp.base.util._
import Event.NESD

/**
 * A change to a [[Project]].
 *
 * All [[Event]]s must be readable from the DB. They must deserialise consistently.
 *
 * Only [[ActiveEvent]]s can be written to the DB.
 */
sealed trait Event
object Event {
  type NESD[A] = NonEmpty[SetDiff[A]]
}

/**
 * Events of which new instances can be created.
 *
 * (Events can be retired over time.)
 */
sealed trait ActiveEvent extends Event

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

case class RepositionField(id: FieldId, newPos: Position[FieldId]) extends ActiveEvent

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
// Content

@CreateGenericData
object GenericReqGD extends GenericData {
  val Title    = defAttr[GenericReqTitle.NonEmptyText]
  val ReqCodes = defAttr[NonEmptySet[ReqCode.Value]]
  val Tags     = defAttr[NonEmptySet[ApplicableTagId]]
  val ImpSrcs  = defAttr[NonEmptySet[ReqId]]
  val ImpTgts  = defAttr[NonEmptySet[ReqId]]
}

case class CreateGenericReq(id: GenericReqId, rt: CustomReqTypeId, vs: GenericReqGD.Values) extends ActiveEvent

// CreateReqCodeGroup

// case class PatchReqTags        (id: ReqId, patch: NESD[ApplicableTagId]) extends ActiveEvent
// case class PatchImplicationSrc (id: ReqId, patch: NESD[ReqId])           extends ActiveEvent
// case class PatchImplicationTgt (id: ReqId, patch: NESD[ReqId])           extends ActiveEvent
// case class PatchReqCodes       (id: ReqId, patch: NESD[ReqCode.Value])   extends ActiveEvent
//
// case class SetGenericReqType   (id: GenericReqId, value: CustomReqTypeId) extends ActiveEvent
// case class SetReqCodeGroupCode (id: ReqCodeId,    value: ReqCode.Value)   extends ActiveEvent
//
// case class SetReqCodeGroupTitle(id: ReqCodeId,                              value: Text.ReqCodeGroupTitle.OptionalText) extends ActiveEvent
// case class SetGenericReqTitle  (id: GenericReqId,                           value: Text.GenericReqTitle.OptionalText)   extends ActiveEvent
// case class SetCustomTextField  (id: ReqId,        fid: CustomField.Text.Id, value: Text.CustomTextField.OptionalText)   extends ActiveEvent