package shipreq.webapp.base.protocol

import shipreq.base.util.{NonEmptySet, SetDiff, UnivEq}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text
import boopickle._, BoopickleMacros._, BinCodecGeneric._, BinCodecData._, AtomPicklers.instances._
import Text.Equality._

/**
 * A command to change a Project's content.
 */
sealed trait UpdateContentCmd
object UpdateContentCmd {

  case class PatchReqTags        (id: ReqId, patch: SetDiff[ApplicableTagId]) extends UpdateContentCmd
  case class PatchImplicationSrc (id: ReqId, patch: SetDiff[ReqId])           extends UpdateContentCmd
  case class PatchImplicationTgt (id: ReqId, patch: SetDiff[ReqId])           extends UpdateContentCmd
  case class PatchReqCodes       (id: ReqId, patch: SetDiff[ReqCode.Value])   extends UpdateContentCmd

  case class SetGenericReqType   (id: GenericReqId, value: CustomReqTypeId) extends UpdateContentCmd
  case class SetReqCodeGroupCode (id: ReqCodeId,    value: ReqCode.Value)   extends UpdateContentCmd

  case class SetReqCodeGroupTitle(id: ReqCodeId,                              value: Text.ReqCodeGroupTitle.OptionalText) extends UpdateContentCmd
  case class SetGenericReqTitle  (id: GenericReqId,                           value: Text.GenericReqTitle.OptionalText)   extends UpdateContentCmd
  case class SetCustomTextField  (id: ReqId,        fid: CustomField.Text.Id, value: Text.CustomTextField.OptionalText)   extends UpdateContentCmd

  case class DeleteReqs         (reqs: NonEmptySet[ReqId], reqCodeGroups: Set[ReqCodeId], reason: Text.DeletionReason.OptionalText) extends UpdateContentCmd
  case class DeleteReqCodeGroups(ids: NonEmptySet[ReqCodeId])                                                                       extends UpdateContentCmd
  case class RestoreContent     (reqs: Set[ReqId], reqCodeGroups: Set[ReqCodeId])                                                   extends UpdateContentCmd

  implicit val cmdEquality: UnivEq[UpdateContentCmd] = UnivEq.deriveAuto

  implicit val picklePatchReqTags        : Pickler[PatchReqTags        ] = pickleCaseClass
  implicit val picklePatchImplicationSrc : Pickler[PatchImplicationSrc ] = pickleCaseClass
  implicit val picklePatchImplicationTgt : Pickler[PatchImplicationTgt ] = pickleCaseClass
  implicit val picklePatchReqCodes       : Pickler[PatchReqCodes       ] = pickleCaseClass
  implicit val pickleSetGenericReqType   : Pickler[SetGenericReqType   ] = pickleCaseClass
  implicit val pickleSetReqCodeGroupCode : Pickler[SetReqCodeGroupCode ] = pickleCaseClass
  implicit val pickleSetReqCodeGroupTitle: Pickler[SetReqCodeGroupTitle] = pickleCaseClass
  implicit val pickleSetGenericReqTitle  : Pickler[SetGenericReqTitle  ] = pickleCaseClass
  implicit val pickleSetCustomTextField  : Pickler[SetCustomTextField  ] = pickleCaseClass
  implicit val pickleDeleteReqs          : Pickler[DeleteReqs          ] = pickleCaseClass
  implicit val pickleDeleteReqCodeGroups : Pickler[DeleteReqCodeGroups ] = pickleCaseClass
  implicit val pickleRestoreContent      : Pickler[RestoreContent      ] = pickleCaseClass
  implicit val pickleCmd                 : Pickler[UpdateContentCmd    ] = pickleADT
}

object UpdateContentFn extends RemoteFn.ToVE[UpdateContentCmd]
