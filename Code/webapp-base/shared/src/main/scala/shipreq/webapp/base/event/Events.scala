package shipreq.webapp.base.event

import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.util._

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


// =====================================================================================================================
//  case class PatchReqTags        (id: ReqId, patch: NESD[ApplicableTagId]) extends ContentUpdate
//  case class PatchImplicationSrc (id: ReqId, patch: NESD[ReqId])           extends ContentUpdate
//  case class PatchImplicationTgt (id: ReqId, patch: NESD[ReqId])           extends ContentUpdate
//  case class PatchReqCodes       (id: ReqId, patch: NESD[ReqCode.Value])   extends ContentUpdate
//
//  case class SetGenericReqType   (id: GenericReqId, value: CustomReqTypeId) extends ContentUpdate
//  case class SetReqCodeGroupCode (id: ReqCodeId,    value: ReqCode.Value)   extends ContentUpdate
//
//  case class SetReqCodeGroupTitle(id: ReqCodeId,                              value: Text.ReqCodeGroupTitle.OptionalText) extends ContentUpdate
//  case class SetGenericReqTitle  (id: GenericReqId,                           value: Text.GenericReqTitle.OptionalText)   extends ContentUpdate
//  case class SetCustomTextField  (id: ReqId,        fid: CustomField.Text.Id, value: Text.CustomTextField.OptionalText)   extends ContentUpdate
//
//  implicit val contentUpdateEquality: UnivEq[ContentUpdate] = { import AutoDerive._; deriveUnivEq }
//}