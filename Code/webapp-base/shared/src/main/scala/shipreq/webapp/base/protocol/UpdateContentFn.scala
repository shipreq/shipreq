package shipreq.webapp.base.protocol

import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text
import boopickle._, BoopickleMacros._, BinCodecGeneric._, BinCodecData._, AtomPicklers.instances._
import Text.Equality._

/**
 * A command to change a Project's content.
 */
sealed abstract class UpdateContentCmd
object UpdateContentCmd {

  case class PatchReqTags        (id: ReqId, patch: NonEmpty[SetDiff[ApplicableTagId]])       extends UpdateContentCmd
  case class PatchImplications   (id: ReqId, dir: Direction, patch: NonEmpty[SetDiff[ReqId]]) extends UpdateContentCmd
  case class PatchReqCodes       (id: ReqId, patch: NonEmpty[SetDiff[ReqCode.Value]])         extends UpdateContentCmd

  case class SetGenericReqType   (id: GenericReqId, value: CustomReqTypeId) extends UpdateContentCmd
  case class SetReqCodeGroupCode (id: ReqCodeId,    value: ReqCode.Value)   extends UpdateContentCmd

  case class SetReqCodeGroupTitle(id: ReqCodeId,                              value: Text.ReqCodeGroupTitle.OptionalText) extends UpdateContentCmd
  case class SetGenericReqTitle  (id: GenericReqId,                           value: Text.GenericReqTitle  .OptionalText) extends UpdateContentCmd
  case class SetUseCaseTitle     (id: UseCaseId,                              value: Text.UseCaseTitle     .OptionalText) extends UpdateContentCmd
  case class SetCustomTextField  (id: ReqId,        fid: CustomField.Text.Id, value: Text.CustomTextField  .OptionalText) extends UpdateContentCmd

  case class DeleteReqs         (reqs: NonEmptySet[ReqId], reqCodeGroups: Set[ReqCodeId], reason: Text.DeletionReason.OptionalText) extends UpdateContentCmd
  case class DeleteReqCodeGroups(ids: NonEmptySet[ReqCodeId])                                                                       extends UpdateContentCmd
  case class RestoreContent     (reqs: Set[ReqId], reqCodeGroups: Set[ReqCodeId])                                                   extends UpdateContentCmd

  sealed abstract class ForUseCaseStep extends UpdateContentCmd

  case class AddUseCaseStep(uc: UseCaseId, f: StaticField.UseCaseStepTree, at: VectorTree.ParentLocation) extends ForUseCaseStep
  case class ShiftUseCaseStepLeft (id: UseCaseStepId) extends ForUseCaseStep
  case class ShiftUseCaseStepRight(id: UseCaseStepId) extends ForUseCaseStep
  case class DeleteUseCaseStep    (id: UseCaseStepId) extends ForUseCaseStep

  implicit val equalForUseCaseStep  : UnivEq[ForUseCaseStep  ] = UnivEq.deriveAuto
  implicit val equalUpdateContentCmd: UnivEq[UpdateContentCmd] = UnivEq.deriveAuto

  implicit val picklePatchReqTags         : Pickler[PatchReqTags         ] = pickleCaseClass
  implicit val picklePatchImplications    : Pickler[PatchImplications    ] = pickleCaseClass
  implicit val picklePatchReqCodes        : Pickler[PatchReqCodes        ] = pickleCaseClass
  implicit val pickleSetGenericReqType    : Pickler[SetGenericReqType    ] = pickleCaseClass
  implicit val pickleSetReqCodeGroupCode  : Pickler[SetReqCodeGroupCode  ] = pickleCaseClass
  implicit val pickleSetReqCodeGroupTitle : Pickler[SetReqCodeGroupTitle ] = pickleCaseClass
  implicit val pickleSetGenericReqTitle   : Pickler[SetGenericReqTitle   ] = pickleCaseClass
  implicit val pickleSetUseCaseTitle      : Pickler[SetUseCaseTitle      ] = pickleCaseClass
  implicit val pickleSetCustomTextField   : Pickler[SetCustomTextField   ] = pickleCaseClass
  implicit val pickleDeleteReqs           : Pickler[DeleteReqs           ] = pickleCaseClass
  implicit val pickleDeleteReqCodeGroups  : Pickler[DeleteReqCodeGroups  ] = pickleCaseClass
  implicit val pickleRestoreContent       : Pickler[RestoreContent       ] = pickleCaseClass
  implicit val pickleAddUseCaseStep       : Pickler[AddUseCaseStep       ] = pickleCaseClass
  implicit val pickleShiftUseCaseStepLeft : Pickler[ShiftUseCaseStepLeft ] = pickleCaseClass
  implicit val pickleShiftUseCaseStepRight: Pickler[ShiftUseCaseStepRight] = pickleCaseClass
  implicit val pickleDeleteUseCaseStep    : Pickler[DeleteUseCaseStep    ] = pickleCaseClass
  implicit val pickleCmd                  : Pickler[UpdateContentCmd     ] = pickleADT
}

object UpdateContentFn extends RemoteFn.ToVE[UpdateContentCmd]
