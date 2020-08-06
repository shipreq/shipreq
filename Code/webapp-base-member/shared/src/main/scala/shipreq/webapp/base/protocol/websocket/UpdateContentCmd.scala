package shipreq.webapp.base.protocol.websocket

import shipreq.base.util.VectorTree.LocationOps
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.UseCaseStepGD
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.text.Text.Equality._

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

  def addUseCaseStepAfter(step: UseCaseStep.Focus): Option[AddUseCaseStep] =
    step.canInsertAfterSelf.option(AddUseCaseStep(step.useCaseId, step.field, step.loc.asParentLoc))

  def ShiftUseCaseStep(id: UseCaseStepId, dir: LeftRight): ForUseCaseStep =
    dir match {
      case LeftRight.Left  => ShiftUseCaseStepLeft (id)
      case LeftRight.Right => ShiftUseCaseStepRight(id)
    }

  implicit def equalForUseCaseStep  : UnivEq[ForUseCaseStep  ] = UnivEq.derive
  implicit def equalUpdateContentCmd: UnivEq[UpdateContentCmd] = UnivEq.derive

  // ===================================================================================================================
  object CodecsV4 {
    import boopickle.DefaultBasic._
    import shipreq.webapp.base.protocol.binary.v1.BaseData._
    import shipreq.webapp.base.protocol.binary.v1.BaseMemberData1._
    import shipreq.webapp.base.protocol.binary.v1.BaseMemberData2._
    import shipreq.webapp.base.protocol.binary.v1.Events._
    import shipreq.webapp.base.protocol.binary.v1.Rev6._
    import shipreq.webapp.base.protocol.binary.v1.Rev6.AtomPicklers.instances._
    // REMEMBER: Don't forget to increment `CodecsVn` if you change these

    private implicit val picklerPatchReqTags: Pickler[PatchReqTags] =
      new Pickler[PatchReqTags] {
        override def pickle(a: PatchReqTags)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.patch)
        }
        override def unpickle(implicit state: UnpickleState): PatchReqTags = {
          val id    = state.unpickle[ReqId]
          val patch = state.unpickle[SetDiff.NE[ApplicableTagId]]
          PatchReqTags(id, patch)
        }
      }

    private implicit val picklerPatchImplications: Pickler[PatchImplications] =
      new Pickler[PatchImplications] {
        override def pickle(a: PatchImplications)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.dir)
          state.pickle(a.patch)
        }
        override def unpickle(implicit state: UnpickleState): PatchImplications = {
          val id    = state.unpickle[ReqId]
          val dir   = state.unpickle[Direction]
          val patch = state.unpickle[SetDiff.NE[ReqId]]
          PatchImplications(id, dir, patch)
        }
      }

    private implicit val picklerPatchReqCodes: Pickler[PatchReqCodes] =
      new Pickler[PatchReqCodes] {
        override def pickle(a: PatchReqCodes)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.patch)
        }
        override def unpickle(implicit state: UnpickleState): PatchReqCodes = {
          val id    = state.unpickle[ReqId]
          val patch = state.unpickle[SetDiff.NE[ReqCode.Value]]
          PatchReqCodes(id, patch)
        }
      }

    private implicit val picklerSetGenericReqType: Pickler[SetGenericReqType] =
      new Pickler[SetGenericReqType] {
        override def pickle(a: SetGenericReqType)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.value)
        }
        override def unpickle(implicit state: UnpickleState): SetGenericReqType = {
          val id    = state.unpickle[GenericReqId]
          val value = state.unpickle[CustomReqTypeId]
          SetGenericReqType(id, value)
        }
      }

    private implicit val picklerSetCodeGroupCode: Pickler[SetCodeGroupCode] =
      new Pickler[SetCodeGroupCode] {
        override def pickle(a: SetCodeGroupCode)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.value)
        }
        override def unpickle(implicit state: UnpickleState): SetCodeGroupCode = {
          val id    = state.unpickle[ReqCodeGroupId]
          val value = state.unpickle[ReqCode.Value]
          SetCodeGroupCode(id, value)
        }
      }

    private implicit val picklerSetCodeGroupTitle: Pickler[SetCodeGroupTitle] =
      new Pickler[SetCodeGroupTitle] {
        override def pickle(a: SetCodeGroupTitle)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.value)
        }
        override def unpickle(implicit state: UnpickleState): SetCodeGroupTitle = {
          val id    = state.unpickle[ReqCodeGroupId]
          val value = state.unpickle[Text.CodeGroupTitle .OptionalText]
          SetCodeGroupTitle(id, value)
        }
      }

    private implicit val picklerSetCustomTextField: Pickler[SetCustomTextField] =
      new Pickler[SetCustomTextField] {
        override def pickle(a: SetCustomTextField)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.fid)
          state.pickle(a.value)
        }
        override def unpickle(implicit state: UnpickleState): SetCustomTextField = {
          val id    = state.unpickle[ReqId]
          val fid   = state.unpickle[CustomField.Text.Id]
          val value = state.unpickle[Text.CustomTextField.OptionalText]
          SetCustomTextField(id, fid, value)
        }
      }

    private implicit val picklerSetGenericReqTitle: Pickler[SetGenericReqTitle] =
      new Pickler[SetGenericReqTitle] {
        override def pickle(a: SetGenericReqTitle)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.value)
        }
        override def unpickle(implicit state: UnpickleState): SetGenericReqTitle = {
          val id    = state.unpickle[GenericReqId]
          val value = state.unpickle[Text.GenericReqTitle.OptionalText]
          SetGenericReqTitle(id, value)
        }
      }

    private implicit val picklerSetUseCaseTitle: Pickler[SetUseCaseTitle] =
      new Pickler[SetUseCaseTitle] {
        override def pickle(a: SetUseCaseTitle)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.value)
        }
        override def unpickle(implicit state: UnpickleState): SetUseCaseTitle = {
          val id    = state.unpickle[UseCaseId]
          val value = state.unpickle[Text.UseCaseTitle   .OptionalText]
          SetUseCaseTitle(id, value)
        }
      }

    private implicit val picklerDeleteReqs: Pickler[DeleteReqs] =
      new Pickler[DeleteReqs] {
        override def pickle(a: DeleteReqs)(implicit state: PickleState): Unit = {
          state.pickle(a.reqs)
          state.pickle(a.codeGroups)
          state.pickle(a.reason)
        }
        override def unpickle(implicit state: UnpickleState): DeleteReqs = {
          val reqs       = state.unpickle[NonEmptySet[ReqId]]
          val codeGroups = state.unpickle[Set[ReqCodeGroupId]]
          val reason     = state.unpickle[Text.DeletionReason.OptionalText]
          DeleteReqs(reqs, codeGroups, reason)
        }
      }

    private implicit val picklerDeleteCodeGroups: Pickler[DeleteCodeGroups] =
      transformPickler(DeleteCodeGroups.apply)(_.ids)

    private implicit val picklerRestoreContent: Pickler[RestoreContent] =
      new Pickler[RestoreContent] {
        override def pickle(a: RestoreContent)(implicit state: PickleState): Unit = {
          state.pickle(a.reqs)
          state.pickle(a.codeGroups)
        }
        override def unpickle(implicit state: UnpickleState): RestoreContent = {
          val reqs       = state.unpickle[Set[ReqId]]
          val codeGroups = state.unpickle[Set[ReqCodeGroupId]]
          RestoreContent(reqs, codeGroups)
        }
      }

    private implicit val picklerAddUseCaseStep: Pickler[AddUseCaseStep] =
      new Pickler[AddUseCaseStep] {
        override def pickle(a: AddUseCaseStep)(implicit state: PickleState): Unit = {
          state.pickle(a.uc)
          state.pickle(a.f)
          state.pickle(a.at)
        }
        override def unpickle(implicit state: UnpickleState): AddUseCaseStep = {
          val uc = state.unpickle[UseCaseId]
          val f  = state.unpickle[StaticField.UseCaseStepTree]
          val at = state.unpickle[VectorTree.ParentLocation]
          AddUseCaseStep(uc, f, at)
        }
      }

    private implicit val picklerShiftUseCaseStepLeft: Pickler[ShiftUseCaseStepLeft] =
      transformPickler(ShiftUseCaseStepLeft.apply)(_.id)

    private implicit val picklerShiftUseCaseStepRight: Pickler[ShiftUseCaseStepRight] =
      transformPickler(ShiftUseCaseStepRight.apply)(_.id)

    private implicit val picklerDeleteUseCaseStep: Pickler[DeleteUseCaseStep] =
      transformPickler(DeleteUseCaseStep.apply)(_.id)

    private implicit val picklerRestoreUseCaseStep: Pickler[RestoreUseCaseStep] =
      transformPickler(RestoreUseCaseStep.apply)(_.id)

    private implicit val picklerUpdateUseCaseStep: Pickler[UpdateUseCaseStep] =
      new Pickler[UpdateUseCaseStep] {
        override def pickle(a: UpdateUseCaseStep)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.vs)
        }
        override def unpickle(implicit state: UnpickleState): UpdateUseCaseStep = {
          val id = state.unpickle[UseCaseStepId]
          val vs = state.unpickle[UseCaseStepGD.NonEmptyValues]
          UpdateUseCaseStep(id, vs)
        }
      }

    implicit val picklerUpdateContentCmd: Pickler[UpdateContentCmd] =
      new Pickler[UpdateContentCmd] {
        private[this] final val KeyAddUseCaseStep        = 0
        private[this] final val KeyDeleteCodeGroups      = 1
        private[this] final val KeyDeleteReqs            = 2
        private[this] final val KeyDeleteUseCaseStep     = 3
        private[this] final val KeyPatchImplications     = 4
        private[this] final val KeyPatchReqCodes         = 5
        private[this] final val KeyPatchReqTags          = 6
        private[this] final val KeyRestoreContent        = 7
        private[this] final val KeyRestoreUseCaseStep    = 8
        private[this] final val KeySetCodeGroupCode      = 9
        private[this] final val KeySetCodeGroupTitle     = 10
        private[this] final val KeySetCustomTextField    = 11
        private[this] final val KeySetGenericReqTitle    = 12
        private[this] final val KeySetGenericReqType     = 13
        private[this] final val KeySetUseCaseTitle       = 14
        private[this] final val KeyShiftUseCaseStepLeft  = 15
        private[this] final val KeyShiftUseCaseStepRight = 16
        private[this] final val KeyUpdateUseCaseStep     = 17
        override def pickle(a: UpdateContentCmd)(implicit state: PickleState): Unit =
          a match {
            case b: AddUseCaseStep        => state.enc.writeByte(KeyAddUseCaseStep       ); state.pickle(b)
            case b: DeleteCodeGroups      => state.enc.writeByte(KeyDeleteCodeGroups     ); state.pickle(b)
            case b: DeleteReqs            => state.enc.writeByte(KeyDeleteReqs           ); state.pickle(b)
            case b: DeleteUseCaseStep     => state.enc.writeByte(KeyDeleteUseCaseStep    ); state.pickle(b)
            case b: PatchImplications     => state.enc.writeByte(KeyPatchImplications    ); state.pickle(b)
            case b: PatchReqCodes         => state.enc.writeByte(KeyPatchReqCodes        ); state.pickle(b)
            case b: PatchReqTags          => state.enc.writeByte(KeyPatchReqTags         ); state.pickle(b)
            case b: RestoreContent        => state.enc.writeByte(KeyRestoreContent       ); state.pickle(b)
            case b: RestoreUseCaseStep    => state.enc.writeByte(KeyRestoreUseCaseStep   ); state.pickle(b)
            case b: SetCodeGroupCode      => state.enc.writeByte(KeySetCodeGroupCode     ); state.pickle(b)
            case b: SetCodeGroupTitle     => state.enc.writeByte(KeySetCodeGroupTitle    ); state.pickle(b)
            case b: SetCustomTextField    => state.enc.writeByte(KeySetCustomTextField   ); state.pickle(b)
            case b: SetGenericReqTitle    => state.enc.writeByte(KeySetGenericReqTitle   ); state.pickle(b)
            case b: SetGenericReqType     => state.enc.writeByte(KeySetGenericReqType    ); state.pickle(b)
            case b: SetUseCaseTitle       => state.enc.writeByte(KeySetUseCaseTitle      ); state.pickle(b)
            case b: ShiftUseCaseStepLeft  => state.enc.writeByte(KeyShiftUseCaseStepLeft ); state.pickle(b)
            case b: ShiftUseCaseStepRight => state.enc.writeByte(KeyShiftUseCaseStepRight); state.pickle(b)
            case b: UpdateUseCaseStep     => state.enc.writeByte(KeyUpdateUseCaseStep    ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): UpdateContentCmd =
          state.dec.readByte match {
            case KeyAddUseCaseStep        => state.unpickle[AddUseCaseStep]
            case KeyDeleteCodeGroups      => state.unpickle[DeleteCodeGroups]
            case KeyDeleteReqs            => state.unpickle[DeleteReqs]
            case KeyDeleteUseCaseStep     => state.unpickle[DeleteUseCaseStep]
            case KeyPatchImplications     => state.unpickle[PatchImplications]
            case KeyPatchReqCodes         => state.unpickle[PatchReqCodes]
            case KeyPatchReqTags          => state.unpickle[PatchReqTags]
            case KeyRestoreContent        => state.unpickle[RestoreContent]
            case KeyRestoreUseCaseStep    => state.unpickle[RestoreUseCaseStep]
            case KeySetCodeGroupCode      => state.unpickle[SetCodeGroupCode]
            case KeySetCodeGroupTitle     => state.unpickle[SetCodeGroupTitle]
            case KeySetCustomTextField    => state.unpickle[SetCustomTextField]
            case KeySetGenericReqTitle    => state.unpickle[SetGenericReqTitle]
            case KeySetGenericReqType     => state.unpickle[SetGenericReqType]
            case KeySetUseCaseTitle       => state.unpickle[SetUseCaseTitle]
            case KeyShiftUseCaseStepLeft  => state.unpickle[ShiftUseCaseStepLeft]
            case KeyShiftUseCaseStepRight => state.unpickle[ShiftUseCaseStepRight]
            case KeyUpdateUseCaseStep     => state.unpickle[UpdateUseCaseStep]
          }
      }

  }
}
