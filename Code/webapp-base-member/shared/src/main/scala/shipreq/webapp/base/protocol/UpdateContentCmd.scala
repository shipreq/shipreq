package shipreq.webapp.base.protocol

import boopickle._
import japgolly.microlibs.nonempty._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.UseCaseStepGD
import shipreq.webapp.base.text.Text
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecBaseData._
import BinCodecMemberData._
import BinCodecEvents.pickleUseCaseStepGD
import AtomPicklers.instances._
import Text.Equality._

/**
 * A command to change a Project's content.
 */
sealed abstract class UpdateContentCmd
object UpdateContentCmd {

  case class PatchReqTags        (id: ReqId, patch: SetDiff.NE[ApplicableTagId])       extends UpdateContentCmd
  case class PatchImplications   (id: ReqId, dir: Direction, patch: SetDiff.NE[ReqId]) extends UpdateContentCmd
  case class PatchReqCodes       (id: ReqId, patch: SetDiff.NE[ReqCode.Value])         extends UpdateContentCmd

  case class SetGenericReqType(id: GenericReqId,   value: CustomReqTypeId) extends UpdateContentCmd
  case class SetCodeGroupCode (id: ReqCodeGroupId, value: ReqCode.Value)   extends UpdateContentCmd

  case class SetCodeGroupTitle (id: ReqCodeGroupId,                         value: Text.CodeGroupTitle .OptionalText) extends UpdateContentCmd
  case class SetCustomTextField(id: ReqId,        fid: CustomField.Text.Id, value: Text.CustomTextField.OptionalText) extends UpdateContentCmd
  case class SetGenericReqTitle(id: GenericReqId,                           value: Text.GenericReqTitle.OptionalText) extends UpdateContentCmd
  case class SetUseCaseTitle   (id: UseCaseId,                              value: Text.UseCaseTitle   .OptionalText) extends UpdateContentCmd

  case class DeleteReqs      (reqs: NonEmptySet[ReqId], codeGroups: Set[ReqCodeGroupId], reason: Text.DeletionReason.OptionalText) extends UpdateContentCmd
  case class DeleteCodeGroups(ids: NonEmptySet[ReqCodeGroupId])                                                                    extends UpdateContentCmd
  case class RestoreContent  (reqs: Set[ReqId], codeGroups: Set[ReqCodeGroupId])                                                   extends UpdateContentCmd

  sealed abstract class ForUseCaseStep extends UpdateContentCmd

  case class AddUseCaseStep(uc: UseCaseId, f: StaticField.UseCaseStepTree, at: VectorTree.ParentLocation) extends ForUseCaseStep
  case class ShiftUseCaseStepLeft (id: UseCaseStepId) extends ForUseCaseStep
  case class ShiftUseCaseStepRight(id: UseCaseStepId) extends ForUseCaseStep
  case class DeleteUseCaseStep    (id: UseCaseStepId) extends ForUseCaseStep
  case class RestoreUseCaseStep   (id: UseCaseStepId) extends ForUseCaseStep
  case class UpdateUseCaseStep    (id: UseCaseStepId, vs: UseCaseStepGD.NonEmptyValues) extends ForUseCaseStep
  def ShiftUseCaseStep(id: UseCaseStepId, dir: LeftRight): ForUseCaseStep =
    dir match {
      case LeftRight.Left  => ShiftUseCaseStepLeft (id)
      case LeftRight.Right => ShiftUseCaseStepRight(id)
    }

  implicit val equalForUseCaseStep  : UnivEq[ForUseCaseStep  ] = UnivEq.derive
  implicit val equalUpdateContentCmd: UnivEq[UpdateContentCmd] = UnivEq.derive

  implicit val picklePatchReqTags         : Pickler[PatchReqTags         ] = pickleCaseClass
  implicit val picklePatchImplications    : Pickler[PatchImplications    ] = pickleCaseClass
  implicit val picklePatchReqCodes        : Pickler[PatchReqCodes        ] = pickleCaseClass
  implicit val pickleSetGenericReqType    : Pickler[SetGenericReqType    ] = pickleCaseClass
  implicit val pickleSetCodeGroupCode     : Pickler[SetCodeGroupCode     ] = pickleCaseClass
  implicit val pickleSetCodeGroupTitle    : Pickler[SetCodeGroupTitle    ] = pickleCaseClass
  implicit val pickleSetGenericReqTitle   : Pickler[SetGenericReqTitle   ] = pickleCaseClass
  implicit val pickleSetUseCaseTitle      : Pickler[SetUseCaseTitle      ] = pickleCaseClass
  implicit val pickleSetCustomTextField   : Pickler[SetCustomTextField   ] = pickleCaseClass
  implicit val pickleDeleteReqs           : Pickler[DeleteReqs           ] = pickleCaseClass
  implicit val pickleDeleteCodeGroups     : Pickler[DeleteCodeGroups     ] = pickleCaseClass
  implicit val pickleRestoreContent       : Pickler[RestoreContent       ] = pickleCaseClass
  implicit val pickleAddUseCaseStep       : Pickler[AddUseCaseStep       ] = pickleCaseClass
  implicit val pickleShiftUseCaseStepLeft : Pickler[ShiftUseCaseStepLeft ] = pickleCaseClass
  implicit val pickleShiftUseCaseStepRight: Pickler[ShiftUseCaseStepRight] = pickleCaseClass
  implicit val pickleDeleteUseCaseStep    : Pickler[DeleteUseCaseStep    ] = pickleCaseClass
  implicit val pickleRestoreUseCaseStep   : Pickler[RestoreUseCaseStep   ] = pickleCaseClass
  implicit val pickleUpdateUseCaseStep    : Pickler[UpdateUseCaseStep    ] = pickleCaseClass
  implicit val pickleCmd                  : Pickler[UpdateContentCmd     ] = pickleADT
}
