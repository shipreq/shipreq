package shipreq.webapp.base.protocol.binary.v1

import japgolly.microlibs.nonempty._
import nyaya.util.Multimap
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.event.RetiredGenericData._
import shipreq.webapp.base.text.Text

object Events {
  import boopickle.DefaultBasic._
  import BaseData._
  import BaseMemberData1._
  import BaseMemberData1.AtomPicklers.instances._
  import BaseMemberData1.ReqTableDataPicklers._

  implicit val picklerEventNonEmptyCustomTextMap    : Pickler[Event.NonEmptyCustomTextMap      ] = pickleNonEmptyMono
  implicit val picklerNonEmptySetApplicableTagId    : Pickler[NonEmptySet[ApplicableTagId]     ] = pickleNES
  implicit val picklerNonEmptySetReqCodeGroupId     : Pickler[NonEmptySet[ReqCodeGroupId]      ] = pickleNES
  implicit val picklerNonEmptySetApReqCodeIdAndValue: Pickler[NonEmptySet[ApReqCodeId.AndValue]] = pickleNES
  implicit val picklerNonEmptySetReqId              : Pickler[NonEmptySet[ReqId]               ] = pickleNES
  implicit val picklerSetDiffUseCaseStepId          : Pickler[SetDiff[UseCaseStepId]           ] = pickleSetDiff
  implicit val picklerSetDiffApplicableTagId        : Pickler[SetDiff[ApplicableTagId]         ] = pickleSetDiff
  implicit val picklerSetDiffReqId                  : Pickler[SetDiff[ReqId]                   ] = pickleSetDiff
  implicit val picklerSetDiffNEUseCaseStepId        : Pickler[SetDiff.NE[UseCaseStepId]        ] = pickleNonEmptyMono
  implicit val picklerSetDiffNEApplicableTagId      : Pickler[SetDiff.NE[ApplicableTagId]      ] = pickleNonEmptyMono
  implicit val picklerSetDiffNEReqId                : Pickler[SetDiff.NE[ReqId]                ] = pickleNonEmptyMono

  implicit val picklerProjectTemplate: Pickler[ProjectTemplate] =
    // Don't use pickleEnum here. When a new case is added the codec-evolution doc needs to be followed
    new Pickler[ProjectTemplate] {
      private[this] final val KeyV1 = 1
      override def pickle(a: ProjectTemplate)(implicit state: PickleState): Unit =
        a match {
          case ProjectTemplate.V1 => state.enc.writeByte(KeyV1)
        }
      override def unpickle(implicit state: UnpickleState): ProjectTemplate =
        state.dec.readByte match {
          case KeyV1 => ProjectTemplate.V1
        }
    }

  // ===================================================================================================================
  // GenericData

  implicit val pickleApplicableTagGDv1: Pickler[ApplicableTagGDv1.NonEmptyValues] = {
    import ApplicableTagGDv1._

    implicit val picklerValueForChildren = transformPickler(ValueForChildren.apply)(_.value)
    implicit val picklerValueForDesc     = transformPickler(ValueForDesc    .apply)(_.value)
    implicit val picklerValueForKey      = transformPickler(ValueForKey     .apply)(_.value)
    implicit val picklerValueForName     = transformPickler(ValueForName    .apply)(_.value)
    implicit val picklerValueForParents  = transformPickler(ValueForParents .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyChildren = 'C'
        private[this] final val KeyDesc     = 'D'
        private[this] final val KeyKey      = 'K'
        private[this] final val KeyName     = 'N'
        private[this] final val KeyParents  = 'P'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForChildren => state.enc.writeByte(KeyChildren); state.pickle(b)
            case b: ValueForDesc     => state.enc.writeByte(KeyDesc    ); state.pickle(b)
            case b: ValueForKey      => state.enc.writeByte(KeyKey     ); state.pickle(b)
            case b: ValueForName     => state.enc.writeByte(KeyName    ); state.pickle(b)
            case b: ValueForParents  => state.enc.writeByte(KeyParents ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyChildren => state.unpickle[ValueForChildren]
            case KeyDesc     => state.unpickle[ValueForDesc]
            case KeyKey      => state.unpickle[ValueForKey]
            case KeyName     => state.unpickle[ValueForName]
            case KeyParents  => state.unpickle[ValueForParents]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit val pickleCodeGroupGD: Pickler[CodeGroupGD.NonEmptyValues] = {
    import CodeGroupGD._

    implicit val picklerValueForCode  = transformPickler(ValueForCode .apply)(_.value)
    implicit val picklerValueForTitle = transformPickler(ValueForTitle.apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyCode  = 'C'
        private[this] final val KeyTitle = 'T'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForCode  => state.enc.writeByte(KeyCode ); state.pickle(b)
            case b: ValueForTitle => state.enc.writeByte(KeyTitle); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyCode  => state.unpickle[ValueForCode ]
            case KeyTitle => state.unpickle[ValueForTitle]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit val pickleCustomImpFieldGDv1: Pickler[CustomImpFieldGDv1.NonEmptyValues] = {
    import CustomImpFieldGDv1._

    implicit val picklerValueForMandatory           = transformPickler(ValueForMandatory         .apply)(_.value)
    implicit val picklerValueForReqTypeId           = transformPickler(ValueForReqTypeId         .apply)(_.value)
    implicit val picklerValueForApplicableReqTypes  = transformPickler(ValueForApplicableReqTypes.apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyMandatory = 'M'
        private[this] final val KeyReqTypeId = 'I'
        private[this] final val KeyReqTypes  = 'R'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForMandatory          => state.enc.writeByte(KeyMandatory); state.pickle(b)
            case b: ValueForReqTypeId          => state.enc.writeByte(KeyReqTypeId); state.pickle(b)
            case b: ValueForApplicableReqTypes => state.enc.writeByte(KeyReqTypes ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyMandatory => state.unpickle[ValueForMandatory]
            case KeyReqTypeId => state.unpickle[ValueForReqTypeId]
            case KeyReqTypes  => state.unpickle[ValueForApplicableReqTypes]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit val pickleCustomIssueTypeGD: Pickler[CustomIssueTypeGD.NonEmptyValues] = {
    import CustomIssueTypeGD._

    implicit val picklerValueForDesc = transformPickler(ValueForDesc.apply)(_.value)
    implicit val picklerValueForKey  = transformPickler(ValueForKey .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyDesc = 'D'
        private[this] final val KeyKey  = 'K'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForDesc => state.enc.writeByte(KeyDesc); state.pickle(b)
            case b: ValueForKey  => state.enc.writeByte(KeyKey ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyDesc => state.unpickle[ValueForDesc]
            case KeyKey  => state.unpickle[ValueForKey]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit val pickleCustomReqTypeGD: Pickler[CustomReqTypeGD.NonEmptyValues] = {
    import CustomReqTypeGD._

    implicit val picklerValueForImp      = transformPickler(ValueForImp     .apply)(_.value)
    implicit val picklerValueForMnemonic = transformPickler(ValueForMnemonic.apply)(_.value)
    implicit val picklerValueForName     = transformPickler(ValueForName    .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyImp      = 'I'
        private[this] final val KeyMnemonic = 'M'
        private[this] final val KeyName     = 'N'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForImp      => state.enc.writeByte(KeyImp     ); state.pickle(b)
            case b: ValueForMnemonic => state.enc.writeByte(KeyMnemonic); state.pickle(b)
            case b: ValueForName     => state.enc.writeByte(KeyName    ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyImp      => state.unpickle[ValueForImp]
            case KeyMnemonic => state.unpickle[ValueForMnemonic]
            case KeyName     => state.unpickle[ValueForName]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit val pickleCustomTagFieldGDv1: Pickler[CustomTagFieldGDv1.NonEmptyValues] = {
    import CustomTagFieldGDv1._

    implicit val picklerValueForMandatory          = transformPickler(ValueForMandatory          .apply)(_.value)
    implicit val picklerValueForApplicableReqTypes = transformPickler(ValueForApplicableReqTypes .apply)(_.value)
    implicit val picklerValueForTagId              = transformPickler(ValueForTagId              .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyMandatory = 'M'
        private[this] final val KeyReqTypes  = 'R'
        private[this] final val KeyTagId     = 'T'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForMandatory          => state.enc.writeByte(KeyMandatory); state.pickle(b)
            case b: ValueForApplicableReqTypes => state.enc.writeByte(KeyReqTypes ); state.pickle(b)
            case b: ValueForTagId              => state.enc.writeByte(KeyTagId    ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyMandatory => state.unpickle[ValueForMandatory]
            case KeyReqTypes  => state.unpickle[ValueForApplicableReqTypes]
            case KeyTagId     => state.unpickle[ValueForTagId]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit val pickleCustomTextFieldGDv1: Pickler[CustomTextFieldGDv1.NonEmptyValues] = {
    import CustomTextFieldGDv1._

    implicit val picklerValueForKey                = transformPickler(ValueForKey               .apply)(_.value)
    implicit val picklerValueForMandatory          = transformPickler(ValueForMandatory         .apply)(_.value)
    implicit val picklerValueForName               = transformPickler(ValueForName              .apply)(_.value)
    implicit val picklerValueForApplicableReqTypes = transformPickler(ValueForApplicableReqTypes.apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyKey       = 'K'
        private[this] final val KeyMandatory = 'M'
        private[this] final val KeyName      = 'N'
        private[this] final val KeyReqTypes  = 'R'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForKey                => state.enc.writeByte(KeyKey      ); state.pickle(b)
            case b: ValueForMandatory          => state.enc.writeByte(KeyMandatory); state.pickle(b)
            case b: ValueForName               => state.enc.writeByte(KeyName     ); state.pickle(b)
            case b: ValueForApplicableReqTypes => state.enc.writeByte(KeyReqTypes ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyKey       => state.unpickle[ValueForKey]
            case KeyMandatory => state.unpickle[ValueForMandatory]
            case KeyName      => state.unpickle[ValueForName]
            case KeyReqTypes  => state.unpickle[ValueForApplicableReqTypes]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit val pickleGenericReqGD: Pickler[GenericReqGD.Values] = {
    import GenericReqGD._

    implicit val picklerValueForCodes      = transformPickler(ValueForCodes     .apply)(_.value)
    implicit val picklerValueForCustomText = transformPickler(ValueForCustomText.apply)(_.value)
    implicit val picklerValueForImpSrcs    = transformPickler(ValueForImpSrcs   .apply)(_.value)
    implicit val picklerValueForImpTgts    = transformPickler(ValueForImpTgts   .apply)(_.value)
    implicit val picklerValueForTags       = transformPickler(ValueForTags      .apply)(_.value)
    implicit val picklerValueForTitle      = transformPickler(ValueForTitle     .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyCodes      = 'C'
        private[this] final val KeyCustomText = 'X'
        private[this] final val KeyImpSrcs    = '>'
        private[this] final val KeyImpTgts    = '<'
        private[this] final val KeyTags       = '#'
        private[this] final val KeyTitle      = 'T'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForCodes      => state.enc.writeByte(KeyCodes     ); state.pickle(b)
            case b: ValueForCustomText => state.enc.writeByte(KeyCustomText); state.pickle(b)
            case b: ValueForImpSrcs    => state.enc.writeByte(KeyImpSrcs   ); state.pickle(b)
            case b: ValueForImpTgts    => state.enc.writeByte(KeyImpTgts   ); state.pickle(b)
            case b: ValueForTags       => state.enc.writeByte(KeyTags      ); state.pickle(b)
            case b: ValueForTitle      => state.enc.writeByte(KeyTitle     ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyCodes      => state.unpickle[ValueForCodes]
            case KeyCustomText => state.unpickle[ValueForCustomText]
            case KeyImpSrcs    => state.unpickle[ValueForImpSrcs]
            case KeyImpTgts    => state.unpickle[ValueForImpTgts]
            case KeyTags       => state.unpickle[ValueForTags]
            case KeyTitle      => state.unpickle[ValueForTitle]
          }
      }

    pickleIMap(emptyValues)
  }

//  implicit val pickleSavedViewGD: Pickler[SavedViewGD.NonEmptyValues] = {
//    import SavedViewGD._
//
//    implicit val picklerValueForColumns    = transformPickler(ValueForColumns   .apply)(_.value)
//    implicit val picklerValueForFilter     = transformPickler(ValueForFilter    .apply)(_.value)
//    implicit val picklerValueForFilterDead = transformPickler(ValueForFilterDead.apply)(_.value)
//    implicit val picklerValueForName       = transformPickler(ValueForName      .apply)(_.value)
//    implicit val picklerValueForOrder      = transformPickler(ValueForOrder     .apply)(_.value)
//
//    implicit val picklerValue: Pickler[Value] =
//      new Pickler[Value] {
//        private[this] final val KeyColumns    = 'C'
//        private[this] final val KeyFilter     = 'F'
//        private[this] final val KeyFilterDead = 'D'
//        private[this] final val KeyName       = 'N'
//        private[this] final val KeyOrder      = 'O'
//        override def pickle(a: Value)(implicit state: PickleState): Unit =
//          a match {
//            case b: ValueForColumns    => state.enc.writeByte(KeyColumns   ); state.pickle(b)
//            case b: ValueForFilter     => state.enc.writeByte(KeyFilter    ); state.pickle(b)
//            case b: ValueForFilterDead => state.enc.writeByte(KeyFilterDead); state.pickle(b)
//            case b: ValueForName       => state.enc.writeByte(KeyName      ); state.pickle(b)
//            case b: ValueForOrder      => state.enc.writeByte(KeyOrder     ); state.pickle(b)
//          }
//        override def unpickle(implicit state: UnpickleState): Value =
//          state.dec.readByte match {
//            case KeyColumns    => state.unpickle[ValueForColumns]
//            case KeyFilter     => state.unpickle[ValueForFilter]
//            case KeyFilterDead => state.unpickle[ValueForFilterDead]
//            case KeyName       => state.unpickle[ValueForName]
//            case KeyOrder      => state.unpickle[ValueForOrder]
//          }
//      }
//
//    val values: Pickler[Values] = pickleIMap(emptyValues)
//    pickleNonEmptyMono[Values](values, implicitly)
//  }

  implicit val pickleTagGroupGD: Pickler[TagGroupGD.NonEmptyValues] = {
    import TagGroupGD._

    implicit val picklerValueForChildren    = transformPickler(ValueForChildren   .apply)(_.value)
    implicit val picklerValueForDesc        = transformPickler(ValueForDesc       .apply)(_.value)
    implicit val picklerValueForExclusivity = transformPickler(ValueForExclusivity.apply)(_.value)
    implicit val picklerValueForName        = transformPickler(ValueForName       .apply)(_.value)
    implicit val picklerValueForParents     = transformPickler(ValueForParents    .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyChildren    = 'C'
        private[this] final val KeyDesc        = 'D'
        private[this] final val KeyExclusivity = 'M'
        private[this] final val KeyName        = 'N'
        private[this] final val KeyParents     = 'P'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForChildren    => state.enc.writeByte(KeyChildren   ); state.pickle(b)
            case b: ValueForDesc        => state.enc.writeByte(KeyDesc       ); state.pickle(b)
            case b: ValueForExclusivity => state.enc.writeByte(KeyExclusivity); state.pickle(b)
            case b: ValueForName        => state.enc.writeByte(KeyName       ); state.pickle(b)
            case b: ValueForParents     => state.enc.writeByte(KeyParents    ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyChildren    => state.unpickle[ValueForChildren]
            case KeyDesc        => state.unpickle[ValueForDesc]
            case KeyExclusivity => state.unpickle[ValueForExclusivity]
            case KeyName        => state.unpickle[ValueForName]
            case KeyParents     => state.unpickle[ValueForParents]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit val pickleUseCaseGD: Pickler[UseCaseGD.Values] = {
    import UseCaseGD._

    implicit val picklerValueForCodes      = transformPickler(ValueForCodes     .apply)(_.value)
    implicit val picklerValueForCustomText = transformPickler(ValueForCustomText.apply)(_.value)
    implicit val picklerValueForImpSrcs    = transformPickler(ValueForImpSrcs   .apply)(_.value)
    implicit val picklerValueForImpTgts    = transformPickler(ValueForImpTgts   .apply)(_.value)
    implicit val picklerValueForTags       = transformPickler(ValueForTags      .apply)(_.value)
    implicit val picklerValueForTitle      = transformPickler(ValueForTitle     .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyCodes      = 'C'
        private[this] final val KeyCustomText = 'X'
        private[this] final val KeyImpSrcs    = '>'
        private[this] final val KeyImpTgts    = '<'
        private[this] final val KeyTags       = '#'
        private[this] final val KeyTitle      = 'T'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForCodes      => state.enc.writeByte(KeyCodes     ); state.pickle(b)
            case b: ValueForCustomText => state.enc.writeByte(KeyCustomText); state.pickle(b)
            case b: ValueForImpSrcs    => state.enc.writeByte(KeyImpSrcs   ); state.pickle(b)
            case b: ValueForImpTgts    => state.enc.writeByte(KeyImpTgts   ); state.pickle(b)
            case b: ValueForTags       => state.enc.writeByte(KeyTags      ); state.pickle(b)
            case b: ValueForTitle      => state.enc.writeByte(KeyTitle     ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyCodes      => state.unpickle[ValueForCodes]
            case KeyCustomText => state.unpickle[ValueForCustomText]
            case KeyImpSrcs    => state.unpickle[ValueForImpSrcs]
            case KeyImpTgts    => state.unpickle[ValueForImpTgts]
            case KeyTags       => state.unpickle[ValueForTags]
            case KeyTitle      => state.unpickle[ValueForTitle]
          }
      }

    pickleIMap(emptyValues)
  }

  implicit val pickleUseCaseStepGD: Pickler[UseCaseStepGD.NonEmptyValues] = {
    import UseCaseStepGD._

    implicit val picklerValueForFlowIn  = transformPickler(ValueForFlowIn .apply)(_.value)
    implicit val picklerValueForFlowOut = transformPickler(ValueForFlowOut.apply)(_.value)
    implicit val picklerValueForTitle   = transformPickler(ValueForTitle  .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyFlowIn  = '<'
        private[this] final val KeyFlowOut = '>'
        private[this] final val KeyTitle   = 'T'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForFlowIn  => state.enc.writeByte(KeyFlowIn ); state.pickle(b)
            case b: ValueForFlowOut => state.enc.writeByte(KeyFlowOut); state.pickle(b)
            case b: ValueForTitle   => state.enc.writeByte(KeyTitle  ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyFlowIn  => state.unpickle[ValueForFlowIn]
            case KeyFlowOut => state.unpickle[ValueForFlowOut]
            case KeyTitle   => state.unpickle[ValueForTitle]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  // ===================================================================================================================
  // Events

  private[v1] implicit val picklerEventProjectNameSet: Pickler[Event.ProjectNameSet] =
    transformPickler(Event.ProjectNameSet.apply)(_.name)

  private[v1] implicit val picklerEventProjectTemplateApply: Pickler[Event.ProjectTemplateApply] =
    transformPickler(Event.ProjectTemplateApply.apply)(_.template)

  private[v1] implicit val picklerEventCustomIssueTypeCreate: Pickler[Event.CustomIssueTypeCreate] =
    new Pickler[Event.CustomIssueTypeCreate] {
      override def pickle(a: Event.CustomIssueTypeCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.CustomIssueTypeCreate = {
        val id = state.unpickle[CustomIssueTypeId]
        val vs = state.unpickle[CustomIssueTypeGD.NonEmptyValues]
        Event.CustomIssueTypeCreate(id, vs)
      }
    }

  private[v1] implicit val picklerEventCustomIssueTypeUpdate: Pickler[Event.CustomIssueTypeUpdate] =
    new Pickler[Event.CustomIssueTypeUpdate] {
      override def pickle(a: Event.CustomIssueTypeUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.CustomIssueTypeUpdate = {
        val id = state.unpickle[CustomIssueTypeId]
        val vs = state.unpickle[CustomIssueTypeGD.NonEmptyValues]
        Event.CustomIssueTypeUpdate(id, vs)
      }
    }

  private[v1] implicit val picklerEventCustomIssueTypeDelete: Pickler[Event.CustomIssueTypeDelete] =
    transformPickler(Event.CustomIssueTypeDelete.apply)(_.id)

  private[v1] implicit val picklerEventCustomIssueTypeRestore: Pickler[Event.CustomIssueTypeRestore] =
    transformPickler(Event.CustomIssueTypeRestore.apply)(_.id)

  private[v1] implicit val picklerEventCustomReqTypeCreate: Pickler[Event.CustomReqTypeCreate] =
    new Pickler[Event.CustomReqTypeCreate] {
      override def pickle(a: Event.CustomReqTypeCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.CustomReqTypeCreate = {
        val id = state.unpickle[CustomReqTypeId]
        val vs = state.unpickle[CustomReqTypeGD.NonEmptyValues]
        Event.CustomReqTypeCreate(id, vs)
      }
    }

  private[v1] implicit val picklerEventCustomReqTypeUpdate: Pickler[Event.CustomReqTypeUpdate] =
    new Pickler[Event.CustomReqTypeUpdate] {
      override def pickle(a: Event.CustomReqTypeUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.CustomReqTypeUpdate = {
        val id = state.unpickle[CustomReqTypeId]
        val vs = state.unpickle[CustomReqTypeGD.NonEmptyValues]
        Event.CustomReqTypeUpdate(id, vs)
      }
    }

  private[v1] implicit val picklerEventCustomReqTypeDelete: Pickler[Event.CustomReqTypeDelete] =
    transformPickler(Event.CustomReqTypeDelete.apply)(_.id)

  private[v1] implicit val picklerEventCustomReqTypeRestore: Pickler[Event.CustomReqTypeRestore] =
    transformPickler(Event.CustomReqTypeRestore.apply)(_.id)

  private[v1] implicit val picklerEventTagDelete: Pickler[Event.TagDelete] =
    transformPickler(Event.TagDelete.apply)(_.id)

  private[v1] implicit val picklerEventTagRestore: Pickler[Event.TagRestore] =
    transformPickler(Event.TagRestore.apply)(_.id)

  private[v1] implicit val picklerEventTagGroupCreate: Pickler[Event.TagGroupCreate] =
    new Pickler[Event.TagGroupCreate] {
      override def pickle(a: Event.TagGroupCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.TagGroupCreate = {
        val id = state.unpickle[TagGroupId]
        val vs = state.unpickle[TagGroupGD.NonEmptyValues]
        Event.TagGroupCreate(id, vs)
      }
    }

  private[v1] implicit val picklerEventTagGroupUpdate: Pickler[Event.TagGroupUpdate] =
    new Pickler[Event.TagGroupUpdate] {
      override def pickle(a: Event.TagGroupUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.TagGroupUpdate = {
        val id = state.unpickle[TagGroupId]
        val vs = state.unpickle[TagGroupGD.NonEmptyValues]
        Event.TagGroupUpdate(id, vs)
      }
    }

  private[v1] implicit val picklerEventApplicableTagCreateV1: Pickler[Event.ApplicableTagCreateV1] =
    new Pickler[Event.ApplicableTagCreateV1] {
      override def pickle(a: Event.ApplicableTagCreateV1)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.ApplicableTagCreateV1 = {
        val id = state.unpickle[ApplicableTagId]
        val vs = state.unpickle[ApplicableTagGDv1.NonEmptyValues]
        Event.ApplicableTagCreateV1(id, vs)
      }
    }

  private[v1] implicit val picklerEventApplicableTagUpdateV1: Pickler[Event.ApplicableTagUpdateV1] =
    new Pickler[Event.ApplicableTagUpdateV1] {
      override def pickle(a: Event.ApplicableTagUpdateV1)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.ApplicableTagUpdateV1 = {
        val id = state.unpickle[ApplicableTagId]
        val vs = state.unpickle[ApplicableTagGDv1.NonEmptyValues]
        Event.ApplicableTagUpdateV1(id, vs)
      }
    }

  private[v1] implicit val picklerEventFieldReposition: Pickler[Event.FieldReposition] =
    new Pickler[Event.FieldReposition] {
      override def pickle(a: Event.FieldReposition)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.newPos)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldReposition = {
        val id     = state.unpickle[FieldId]
        val newPos = state.unpickle[Option[FieldId]]
        Event.FieldReposition(id, newPos)
      }
    }

  private[v1] implicit val picklerEventFieldStaticAdd: Pickler[Event.FieldStaticAdd] =
    transformPickler(Event.FieldStaticAdd.apply)(_.f)

  private[v1] implicit val picklerEventFieldStaticRemove: Pickler[Event.FieldStaticRemove] =
    transformPickler(Event.FieldStaticRemove.apply)(_.f)

  private[v1] implicit val picklerEventFieldCustomDelete: Pickler[Event.FieldCustomDelete] =
    transformPickler(Event.FieldCustomDelete.apply)(_.id)

  private[v1] implicit val picklerEventFieldCustomRestore: Pickler[Event.FieldCustomRestore] =
    transformPickler(Event.FieldCustomRestore.apply)(_.id)

  private[v1] implicit val picklerEventFieldCustomTextCreateV1: Pickler[Event.FieldCustomTextCreateV1] =
    new Pickler[Event.FieldCustomTextCreateV1] {
      override def pickle(a: Event.FieldCustomTextCreateV1)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomTextCreateV1 = {
        val id = state.unpickle[CustomField.Text.Id]
        val vs = state.unpickle[CustomTextFieldGDv1.NonEmptyValues]
        Event.FieldCustomTextCreateV1(id, vs)
      }
    }

  private[v1] implicit val picklerEventFieldCustomTextUpdateV1: Pickler[Event.FieldCustomTextUpdateV1] =
    new Pickler[Event.FieldCustomTextUpdateV1] {
      override def pickle(a: Event.FieldCustomTextUpdateV1)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomTextUpdateV1 = {
        val id = state.unpickle[CustomField.Text.Id]
        val vs = state.unpickle[CustomTextFieldGDv1.NonEmptyValues]
        Event.FieldCustomTextUpdateV1(id, vs)
      }
    }

  private[v1] implicit val picklerEventFieldCustomTagCreateV1: Pickler[Event.FieldCustomTagCreateV1] =
    new Pickler[Event.FieldCustomTagCreateV1] {
      override def pickle(a: Event.FieldCustomTagCreateV1)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomTagCreateV1 = {
        val id = state.unpickle[CustomField.Tag.Id]
        val vs = state.unpickle[CustomTagFieldGDv1.NonEmptyValues]
        Event.FieldCustomTagCreateV1(id, vs)
      }
    }

  private[v1] implicit val picklerEventFieldCustomTagUpdateV1: Pickler[Event.FieldCustomTagUpdateV1] =
    new Pickler[Event.FieldCustomTagUpdateV1] {
      override def pickle(a: Event.FieldCustomTagUpdateV1)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomTagUpdateV1 = {
        val id = state.unpickle[CustomField.Tag.Id]
        val vs = state.unpickle[CustomTagFieldGDv1.NonEmptyValues]
        Event.FieldCustomTagUpdateV1(id, vs)
      }
    }

  private[v1] implicit val picklerEventFieldCustomImpCreateV1: Pickler[Event.FieldCustomImpCreateV1] =
    new Pickler[Event.FieldCustomImpCreateV1] {
      override def pickle(a: Event.FieldCustomImpCreateV1)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomImpCreateV1 = {
        val id = state.unpickle[CustomField.Implication.Id]
        val vs = state.unpickle[CustomImpFieldGDv1.NonEmptyValues]
        Event.FieldCustomImpCreateV1(id, vs)
      }
    }

  private[v1] implicit val picklerEventFieldCustomImpUpdateV1: Pickler[Event.FieldCustomImpUpdateV1] =
    new Pickler[Event.FieldCustomImpUpdateV1] {
      override def pickle(a: Event.FieldCustomImpUpdateV1)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomImpUpdateV1 = {
        val id = state.unpickle[CustomField.Implication.Id]
        val vs = state.unpickle[CustomImpFieldGDv1.NonEmptyValues]
        Event.FieldCustomImpUpdateV1(id, vs)
      }
    }

  private[v1] implicit val picklerEventGenericReqCreate: Pickler[Event.GenericReqCreate] =
    new Pickler[Event.GenericReqCreate] {
      override def pickle(a: Event.GenericReqCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.rt)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.GenericReqCreate = {
        val id = state.unpickle[GenericReqId]
        val rt = state.unpickle[CustomReqTypeId]
        val vs = state.unpickle[GenericReqGD.Values]
        Event.GenericReqCreate(id, rt, vs)
      }
    }

  private[v1] implicit val picklerEventGenericReqTypeSet: Pickler[Event.GenericReqTypeSet] =
    new Pickler[Event.GenericReqTypeSet] {
      override def pickle(a: Event.GenericReqTypeSet)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.value)
      }
      override def unpickle(implicit state: UnpickleState): Event.GenericReqTypeSet = {
        val id    = state.unpickle[GenericReqId]
        val value = state.unpickle[CustomReqTypeId]
        Event.GenericReqTypeSet(id, value)
      }
    }

  private[v1] implicit val picklerEventGenericReqTitleSet: Pickler[Event.GenericReqTitleSet] =
    new Pickler[Event.GenericReqTitleSet] {
      override def pickle(a: Event.GenericReqTitleSet)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.value)
      }
      override def unpickle(implicit state: UnpickleState): Event.GenericReqTitleSet = {
        val id    = state.unpickle[GenericReqId]
        val value = state.unpickle[Text.GenericReqTitle.OptionalText]
        Event.GenericReqTitleSet(id, value)
      }
    }

  private[v1] implicit val picklerEventUseCaseCreate: Pickler[Event.UseCaseCreate] =
    new Pickler[Event.UseCaseCreate] {
      override def pickle(a: Event.UseCaseCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.stepId)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.UseCaseCreate = {
        val id     = state.unpickle[UseCaseId]
        val stepId = state.unpickle[UseCaseStepId]
        val vs     = state.unpickle[UseCaseGD.Values]
        Event.UseCaseCreate(id, stepId, vs)
      }
    }

  private[v1] implicit val picklerEventUseCaseTitleSet: Pickler[Event.UseCaseTitleSet] =
    new Pickler[Event.UseCaseTitleSet] {
      override def pickle(a: Event.UseCaseTitleSet)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.value)
      }
      override def unpickle(implicit state: UnpickleState): Event.UseCaseTitleSet = {
        val id    = state.unpickle[UseCaseId]
        val value = state.unpickle[Text.UseCaseTitle.OptionalText]
        Event.UseCaseTitleSet(id, value)
      }
    }

  private[v1] implicit val picklerEventUseCaseStepCreate: Pickler[Event.UseCaseStepCreate] =
    new Pickler[Event.UseCaseStepCreate] {
      override def pickle(a: Event.UseCaseStepCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.ucId)
        state.pickle(a.field)
        state.pickle(a.at)
      }
      override def unpickle(implicit state: UnpickleState): Event.UseCaseStepCreate = {
        val id    = state.unpickle[UseCaseStepId]
        val ucId  = state.unpickle[UseCaseId]
        val field = state.unpickle[StaticField.UseCaseStepTree]
        val at    = state.unpickle[VectorTree.ParentLocation]
        Event.UseCaseStepCreate(id, ucId, field, at)
      }
    }

  private[v1] implicit val picklerEventUseCaseStepUpdate: Pickler[Event.UseCaseStepUpdate] =
    new Pickler[Event.UseCaseStepUpdate] {
      override def pickle(a: Event.UseCaseStepUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.UseCaseStepUpdate = {
        val id = state.unpickle[UseCaseStepId]
        val vs = state.unpickle[UseCaseStepGD.NonEmptyValues]
        Event.UseCaseStepUpdate(id, vs)
      }
    }

  private[v1] implicit val picklerEventUseCaseStepShiftLeft: Pickler[Event.UseCaseStepShiftLeft] =
    transformPickler(Event.UseCaseStepShiftLeft.apply)(_.id)

  private[v1] implicit val picklerEventUseCaseStepShiftRight: Pickler[Event.UseCaseStepShiftRight] =
    transformPickler(Event.UseCaseStepShiftRight.apply)(_.id)

  private[v1] implicit val picklerEventUseCaseStepDelete: Pickler[Event.UseCaseStepDelete] =
    transformPickler(Event.UseCaseStepDelete.apply)(_.id)

  private[v1] implicit val picklerEventUseCaseStepRestore: Pickler[Event.UseCaseStepRestore] =
    transformPickler(Event.UseCaseStepRestore.apply)(_.id)

  private[v1] implicit val picklerEventCodeGroupCreate: Pickler[Event.CodeGroupCreate] =
    new Pickler[Event.CodeGroupCreate] {
      override def pickle(a: Event.CodeGroupCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.CodeGroupCreate = {
        val id = state.unpickle[ReqCodeGroupId]
        val vs = state.unpickle[CodeGroupGD.NonEmptyValues]
        Event.CodeGroupCreate(id, vs)
      }
    }

  private[v1] implicit val picklerEventCodeGroupUpdate: Pickler[Event.CodeGroupUpdate] =
    new Pickler[Event.CodeGroupUpdate] {
      override def pickle(a: Event.CodeGroupUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.CodeGroupUpdate = {
        val id = state.unpickle[ReqCodeGroupId]
        val vs = state.unpickle[CodeGroupGD.NonEmptyValues]
        Event.CodeGroupUpdate(id, vs)
      }
    }

  private[v1] implicit val picklerEventCodeGroupsDelete: Pickler[Event.CodeGroupsDelete] =
    transformPickler(Event.CodeGroupsDelete.apply)(_.ids)

  private[v1] implicit val picklerEventReqCodesPatch: Pickler[Event.ReqCodesPatch] =
    new Pickler[Event.ReqCodesPatch] {
      override def pickle(a: Event.ReqCodesPatch)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.remove)
        state.pickle(a.restore)
        state.pickle(a.add)
      }
      override def unpickle(implicit state: UnpickleState): Event.ReqCodesPatch = {
        val id      = state.unpickle[ReqId]
        val remove  = state.unpickle[Set[ApReqCodeId]]
        val restore = state.unpickle[Set[ApReqCodeId]]
        val add     = state.unpickle[Multimap[ReqCode.Value, Set, ApReqCodeId]]
        Event.ReqCodesPatch(id, remove, restore, add)
      }
    }

  private[v1] implicit val picklerEventReqTagsPatch: Pickler[Event.ReqTagsPatch] =
    new Pickler[Event.ReqTagsPatch] {
      override def pickle(a: Event.ReqTagsPatch)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.patch)
      }
      override def unpickle(implicit state: UnpickleState): Event.ReqTagsPatch = {
        val id    = state.unpickle[ReqId]
        val patch = state.unpickle[SetDiff.NE[ApplicableTagId]]
        Event.ReqTagsPatch(id, patch)
      }
    }

  private[v1] implicit val picklerEventReqImplicationsPatch: Pickler[Event.ReqImplicationsPatch] =
    new Pickler[Event.ReqImplicationsPatch] {
      override def pickle(a: Event.ReqImplicationsPatch)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.dir)
        state.pickle(a.patch)
      }
      override def unpickle(implicit state: UnpickleState): Event.ReqImplicationsPatch = {
        val id    = state.unpickle[ReqId]
        val dir   = state.unpickle[Direction]
        val patch = state.unpickle[SetDiff.NE[ReqId]]
        Event.ReqImplicationsPatch(id, dir, patch)
      }
    }

  private[v1] implicit val picklerEventReqFieldCustomTextSet: Pickler[Event.ReqFieldCustomTextSet] =
    new Pickler[Event.ReqFieldCustomTextSet] {
      override def pickle(a: Event.ReqFieldCustomTextSet)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.fid)
        state.pickle(a.value)
      }
      override def unpickle(implicit state: UnpickleState): Event.ReqFieldCustomTextSet = {
        val id    = state.unpickle[ReqId]
        val fid   = state.unpickle[CustomField.Text.Id]
        val value = state.unpickle[Text.CustomTextField.OptionalText]
        Event.ReqFieldCustomTextSet(id, fid, value)
      }
    }

  private[v1] implicit val picklerEventReqsDelete: Pickler[Event.ReqsDelete] =
    new Pickler[Event.ReqsDelete] {
      override def pickle(a: Event.ReqsDelete)(implicit state: PickleState): Unit = {
        state.pickle(a.reqs)
        state.pickle(a.codeGroups)
        state.pickle(a.reason)
      }
      override def unpickle(implicit state: UnpickleState): Event.ReqsDelete = {
        val reqs       = state.unpickle[NonEmptySet[ReqId]]
        val codeGroups = state.unpickle[Set[ReqCodeGroupId]]
        val reason     = state.unpickle[Text.DeletionReason.OptionalText]
        Event.ReqsDelete(reqs, codeGroups, reason)
      }
    }

  private[v1] implicit val picklerEventContentRestore: Pickler[Event.ContentRestore] =
    new Pickler[Event.ContentRestore] {
      override def pickle(a: Event.ContentRestore)(implicit state: PickleState): Unit = {
        state.pickle(a.reqs)
        state.pickle(a.codeGroups)
      }
      override def unpickle(implicit state: UnpickleState): Event.ContentRestore = {
        val reqs       = state.unpickle[Set[ReqId]]
        val codeGroups = state.unpickle[Set[ReqCodeGroupId]]
        Event.ContentRestore(reqs, codeGroups)
      }
    }

  private[v1] implicit val picklerEventManualIssueCreate: Pickler[Event.ManualIssueCreate] =
    new Pickler[Event.ManualIssueCreate] {
      override def pickle(a: Event.ManualIssueCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.text)
      }
      override def unpickle(implicit state: UnpickleState): Event.ManualIssueCreate = {
        val id   = state.unpickle[ManualIssueId]
        val text = state.unpickle[Text.ManualIssue.NonEmptyText]
        Event.ManualIssueCreate(id, text)
      }
    }

  private[v1] implicit val picklerEventManualIssueUpdate: Pickler[Event.ManualIssueUpdate] =
    new Pickler[Event.ManualIssueUpdate] {
      override def pickle(a: Event.ManualIssueUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.text)
      }
      override def unpickle(implicit state: UnpickleState): Event.ManualIssueUpdate = {
        val id   = state.unpickle[ManualIssueId]
        val text = state.unpickle[Text.ManualIssue.NonEmptyText]
        Event.ManualIssueUpdate(id, text)
      }
    }

  private[v1] implicit val picklerEventManualIssueDelete: Pickler[Event.ManualIssueDelete] =
    transformPickler(Event.ManualIssueDelete.apply)(_.id)

  // Replaced by v1.1
//  private[v1] implicit val picklerEventSavedViewCreate: Pickler[Event.SavedViewCreate] =
//    new Pickler[Event.SavedViewCreate] {
//      override def pickle(a: Event.SavedViewCreate)(implicit state: PickleState): Unit = {
//        state.pickle(a.id)
//        state.pickle(a.name)
//        state.pickle(a.columns)
//        state.pickle(a.order)
//        state.pickle(a.filterDead)
//        state.pickle(a.filter)
//      }
//      override def unpickle(implicit state: UnpickleState): Event.SavedViewCreate = {
//        val id         = state.unpickle[SavedView.Id]
//        val name       = state.unpickle[SavedView.Name]
//        val columns    = state.unpickle[NonEmptyVector[reqtable.Column]]
//        val order      = state.unpickle[reqtable.SortCriteria]
//        val filterDead = state.unpickle[FilterDead]
//        val filter     = state.unpickle[Option[Filter.Valid]]
//        Event.SavedViewCreate(id, name, columns, order, filterDead, filter)
//      }
//    }
//
//  private[v1] implicit val picklerEventSavedViewUpdate: Pickler[Event.SavedViewUpdate] =
//    new Pickler[Event.SavedViewUpdate] {
//      override def pickle(a: Event.SavedViewUpdate)(implicit state: PickleState): Unit = {
//        state.pickle(a.id)
//        state.pickle(a.vs)
//      }
//      override def unpickle(implicit state: UnpickleState): Event.SavedViewUpdate = {
//        val id = state.unpickle[SavedView.Id]
//        val vs = state.unpickle[SavedViewGD.NonEmptyValues]
//        Event.SavedViewUpdate(id, vs)
//      }
//    }

  private[v1] implicit val picklerEventSavedViewDelete: Pickler[Event.SavedViewDelete] =
    transformPickler(Event.SavedViewDelete.apply)(_.id)

  private[v1] implicit val picklerEventSavedViewDefaultSet: Pickler[Event.SavedViewDefaultSet] =
    transformPickler(Event.SavedViewDefaultSet.apply)(_.id)

  // ===================================================================================================================

  // Replaced by v1.1
  // implicit val picklerEvent: Pickler[Event] =
  //   new Pickler[Event] {
  //     import Event._
  //     private[this] final val KeyApplicableTagCreate     = 0
  //     private[this] final val KeyApplicableTagUpdate     = 1
  //     private[this] final val KeyCodeGroupCreate         = 2
  //     private[this] final val KeyCodeGroupUpdate         = 3
  //     private[this] final val KeyCodeGroupsDelete        = 4
  //     private[this] final val KeyContentRestore          = 5
  //     private[this] final val KeyCustomIssueTypeCreate   = 6
  //     private[this] final val KeyCustomIssueTypeDelete   = 7
  //     private[this] final val KeyCustomIssueTypeRestore  = 8
  //     private[this] final val KeyCustomIssueTypeUpdate   = 9
  //     private[this] final val KeyCustomReqTypeCreate     = 10
  //     private[this] final val KeyCustomReqTypeDelete     = 11
  //     private[this] final val KeyCustomReqTypeRestore    = 12
  //     private[this] final val KeyCustomReqTypeUpdate     = 13
  //     private[this] final val KeyFieldCustomDelete       = 14
  //     private[this] final val KeyFieldCustomImpCreateV1  = 15
  //     private[this] final val KeyFieldCustomImpUpdateV1  = 16
  //     private[this] final val KeyFieldCustomRestore      = 17
  //     private[this] final val KeyFieldCustomTagCreateV1  = 18
  //     private[this] final val KeyFieldCustomTagUpdateV1  = 19
  //     private[this] final val KeyFieldCustomTextCreate   = 20
  //     private[this] final val KeyFieldCustomTextUpdateV1 = 21
  //     private[this] final val KeyFieldReposition         = 22
  //     private[this] final val KeyFieldStaticAdd          = 23
  //     private[this] final val KeyFieldStaticRemove       = 24
  //     private[this] final val KeyGenericReqCreate        = 25
  //     private[this] final val KeyGenericReqTitleSet      = 26
  //     private[this] final val KeyGenericReqTypeSet       = 27
  //     private[this] final val KeyManualIssueCreate       = 28
  //     private[this] final val KeyManualIssueDelete       = 29
  //     private[this] final val KeyManualIssueUpdate       = 30
  //     private[this] final val KeyProjectNameSet          = 31
  //     private[this] final val KeyProjectTemplateApply    = 32
  //     private[this] final val KeyReqCodesPatch           = 33
  //     private[this] final val KeyReqFieldCustomTextSet   = 34
  //     private[this] final val KeyReqImplicationsPatch    = 35
  //     private[this] final val KeyReqTagsPatch            = 36
  //     private[this] final val KeyReqsDelete              = 37
  //     private[this] final val KeySavedViewCreate         = 38
  //     private[this] final val KeySavedViewDefaultSet     = 39
  //     private[this] final val KeySavedViewDelete         = 40
  //     private[this] final val KeySavedViewUpdate         = 41
  //     private[this] final val KeyTagDelete               = 42
  //     private[this] final val KeyTagGroupCreate          = 43
  //     private[this] final val KeyTagGroupUpdate          = 44
  //     private[this] final val KeyTagRestore              = 45
  //     private[this] final val KeyUseCaseCreate           = 46
  //     private[this] final val KeyUseCaseStepCreate       = 47
  //     private[this] final val KeyUseCaseStepDelete       = 48
  //     private[this] final val KeyUseCaseStepRestore      = 49
  //     private[this] final val KeyUseCaseStepShiftLeft    = 50
  //     private[this] final val KeyUseCaseStepShiftRight   = 51
  //     private[this] final val KeyUseCaseStepUpdate       = 52
  //     private[this] final val KeyUseCaseTitleSet         = 53
  //
  //     override def pickle(a: Event)(implicit state: PickleState): Unit =
  //         case b: ApplicableTagCreateV1   => state.enc.writeByte(KeyApplicableTagCreate    ); state.pickle(b)
  //         case b: ApplicableTagUpdateV1   => state.enc.writeByte(KeyApplicableTagUpdate    ); state.pickle(b)
  //         case b: CodeGroupCreate         => state.enc.writeByte(KeyCodeGroupCreate        ); state.pickle(b)
  //         case b: CodeGroupUpdate         => state.enc.writeByte(KeyCodeGroupUpdate        ); state.pickle(b)
  //         case b: CodeGroupsDelete        => state.enc.writeByte(KeyCodeGroupsDelete       ); state.pickle(b)
  //         case b: ContentRestore          => state.enc.writeByte(KeyContentRestore         ); state.pickle(b)
  //         case b: CustomIssueTypeCreate   => state.enc.writeByte(KeyCustomIssueTypeCreate  ); state.pickle(b)
  //         case b: CustomIssueTypeDelete   => state.enc.writeByte(KeyCustomIssueTypeDelete  ); state.pickle(b)
  //         case b: CustomIssueTypeRestore  => state.enc.writeByte(KeyCustomIssueTypeRestore ); state.pickle(b)
  //         case b: CustomIssueTypeUpdate   => state.enc.writeByte(KeyCustomIssueTypeUpdate  ); state.pickle(b)
  //         case b: CustomReqTypeCreate     => state.enc.writeByte(KeyCustomReqTypeCreate    ); state.pickle(b)
  //         case b: CustomReqTypeDelete     => state.enc.writeByte(KeyCustomReqTypeDelete    ); state.pickle(b)
  //         case b: CustomReqTypeRestore    => state.enc.writeByte(KeyCustomReqTypeRestore   ); state.pickle(b)
  //         case b: CustomReqTypeUpdate     => state.enc.writeByte(KeyCustomReqTypeUpdate    ); state.pickle(b)
  //         case b: FieldCustomDelete       => state.enc.writeByte(KeyFieldCustomDelete      ); state.pickle(b)
  //         case b: FieldCustomImpCreateV1  => state.enc.writeByte(KeyFieldCustomImpCreateV1 ); state.pickle(b)
  //         case b: FieldCustomImpUpdateV1  => state.enc.writeByte(KeyFieldCustomImpUpdateV1 ); state.pickle(b)
  //         case b: FieldCustomRestore      => state.enc.writeByte(KeyFieldCustomRestore     ); state.pickle(b)
  //         case b: FieldCustomTagCreateV1  => state.enc.writeByte(KeyFieldCustomTagCreateV1 ); state.pickle(b)
  //         case b: FieldCustomTagUpdateV1  => state.enc.writeByte(KeyFieldCustomTagUpdateV1 ); state.pickle(b)
  //         case b: FieldCustomTextCreate   => state.enc.writeByte(KeyFieldCustomTextCreate  ); state.pickle(b)
  //         case b: FieldCustomTextUpdateV1 => state.enc.writeByte(KeyFieldCustomTextUpdateV1); state.pickle(b)
  //         case b: FieldReposition         => state.enc.writeByte(KeyFieldReposition        ); state.pickle(b)
  //         case b: FieldStaticAdd          => state.enc.writeByte(KeyFieldStaticAdd         ); state.pickle(b)
  //         case b: FieldStaticRemove       => state.enc.writeByte(KeyFieldStaticRemove      ); state.pickle(b)
  //         case b: GenericReqCreate        => state.enc.writeByte(KeyGenericReqCreate       ); state.pickle(b)
  //         case b: GenericReqTitleSet      => state.enc.writeByte(KeyGenericReqTitleSet     ); state.pickle(b)
  //         case b: GenericReqTypeSet       => state.enc.writeByte(KeyGenericReqTypeSet      ); state.pickle(b)
  //         case b: ManualIssueCreate       => state.enc.writeByte(KeyManualIssueCreate      ); state.pickle(b)
  //         case b: ManualIssueDelete       => state.enc.writeByte(KeyManualIssueDelete      ); state.pickle(b)
  //         case b: ManualIssueUpdate       => state.enc.writeByte(KeyManualIssueUpdate      ); state.pickle(b)
  //         case b: ProjectNameSet          => state.enc.writeByte(KeyProjectNameSet         ); state.pickle(b)
  //         case b: ProjectTemplateApply    => state.enc.writeByte(KeyProjectTemplateApply   ); state.pickle(b)
  //         case b: ReqCodesPatch           => state.enc.writeByte(KeyReqCodesPatch          ); state.pickle(b)
  //         case b: ReqFieldCustomTextSet   => state.enc.writeByte(KeyReqFieldCustomTextSet  ); state.pickle(b)
  //         case b: ReqImplicationsPatch    => state.enc.writeByte(KeyReqImplicationsPatch   ); state.pickle(b)
  //         case b: ReqTagsPatch            => state.enc.writeByte(KeyReqTagsPatch           ); state.pickle(b)
  //         case b: ReqsDelete              => state.enc.writeByte(KeyReqsDelete             ); state.pickle(b)
  //         case b: SavedViewCreate         => state.enc.writeByte(KeySavedViewCreate        ); state.pickle(b)
  //         case b: SavedViewDefaultSet     => state.enc.writeByte(KeySavedViewDefaultSet    ); state.pickle(b)
  //         case b: SavedViewDelete         => state.enc.writeByte(KeySavedViewDelete        ); state.pickle(b)
  //         case b: SavedViewUpdate         => state.enc.writeByte(KeySavedViewUpdate        ); state.pickle(b)
  //         case b: TagDelete               => state.enc.writeByte(KeyTagDelete              ); state.pickle(b)
  //         case b: TagGroupCreate          => state.enc.writeByte(KeyTagGroupCreate         ); state.pickle(b)
  //         case b: TagGroupUpdate          => state.enc.writeByte(KeyTagGroupUpdate         ); state.pickle(b)
  //         case b: TagRestore              => state.enc.writeByte(KeyTagRestore             ); state.pickle(b)
  //         case b: UseCaseCreate           => state.enc.writeByte(KeyUseCaseCreate          ); state.pickle(b)
  //         case b: UseCaseStepCreate       => state.enc.writeByte(KeyUseCaseStepCreate      ); state.pickle(b)
  //         case b: UseCaseStepDelete       => state.enc.writeByte(KeyUseCaseStepDelete      ); state.pickle(b)
  //         case b: UseCaseStepRestore      => state.enc.writeByte(KeyUseCaseStepRestore     ); state.pickle(b)
  //         case b: UseCaseStepShiftLeft    => state.enc.writeByte(KeyUseCaseStepShiftLeft   ); state.pickle(b)
  //         case b: UseCaseStepShiftRight   => state.enc.writeByte(KeyUseCaseStepShiftRight  ); state.pickle(b)
  //         case b: UseCaseStepUpdate       => state.enc.writeByte(KeyUseCaseStepUpdate      ); state.pickle(b)
  //         case b: UseCaseTitleSet         => state.enc.writeByte(KeyUseCaseTitleSet        ); state.pickle(b)
  //       }
  //
  //     override def unpickle(implicit state: UnpickleState): Event =
  //         case KeyApplicableTagCreate     => state.unpickle[ApplicableTagCreateV1]
  //         case KeyApplicableTagUpdate     => state.unpickle[ApplicableTagUpdateV1]
  //         case KeyCodeGroupCreate         => state.unpickle[CodeGroupCreate]
  //         case KeyCodeGroupUpdate         => state.unpickle[CodeGroupUpdate]
  //         case KeyCodeGroupsDelete        => state.unpickle[CodeGroupsDelete]
  //         case KeyContentRestore          => state.unpickle[ContentRestore]
  //         case KeyCustomIssueTypeCreate   => state.unpickle[CustomIssueTypeCreate]
  //         case KeyCustomIssueTypeDelete   => state.unpickle[CustomIssueTypeDelete]
  //         case KeyCustomIssueTypeRestore  => state.unpickle[CustomIssueTypeRestore]
  //         case KeyCustomIssueTypeUpdate   => state.unpickle[CustomIssueTypeUpdate]
  //         case KeyCustomReqTypeCreate     => state.unpickle[CustomReqTypeCreate]
  //         case KeyCustomReqTypeDelete     => state.unpickle[CustomReqTypeDelete]
  //         case KeyCustomReqTypeRestore    => state.unpickle[CustomReqTypeRestore]
  //         case KeyCustomReqTypeUpdate     => state.unpickle[CustomReqTypeUpdate]
  //         case KeyFieldCustomDelete       => state.unpickle[FieldCustomDelete]
  //         case KeyFieldCustomImpCreateV1  => state.unpickle[FieldCustomImpCreateV1]
  //         case KeyFieldCustomImpUpdateV1  => state.unpickle[FieldCustomImpUpdateV1]
  //         case KeyFieldCustomRestore      => state.unpickle[FieldCustomRestore]
  //         case KeyFieldCustomTagCreateV1  => state.unpickle[FieldCustomTagCreateV1]
  //         case KeyFieldCustomTagUpdateV1  => state.unpickle[FieldCustomTagUpdateV1]
  //         case KeyFieldCustomTextCreate   => state.unpickle[FieldCustomTextCreate]
  //         case KeyFieldCustomTextUpdateV1 => state.unpickle[FieldCustomTextUpdateV1]
  //         case KeyFieldReposition         => state.unpickle[FieldReposition]
  //         case KeyFieldStaticAdd          => state.unpickle[FieldStaticAdd]
  //         case KeyFieldStaticRemove       => state.unpickle[FieldStaticRemove]
  //         case KeyGenericReqCreate        => state.unpickle[GenericReqCreate]
  //         case KeyGenericReqTitleSet      => state.unpickle[GenericReqTitleSet]
  //         case KeyGenericReqTypeSet       => state.unpickle[GenericReqTypeSet]
  //         case KeyManualIssueCreate       => state.unpickle[ManualIssueCreate]
  //         case KeyManualIssueDelete       => state.unpickle[ManualIssueDelete]
  //         case KeyManualIssueUpdate       => state.unpickle[ManualIssueUpdate]
  //         case KeyProjectNameSet          => state.unpickle[ProjectNameSet]
  //         case KeyProjectTemplateApply    => state.unpickle[ProjectTemplateApply]
  //         case KeyReqCodesPatch           => state.unpickle[ReqCodesPatch]
  //         case KeyReqFieldCustomTextSet   => state.unpickle[ReqFieldCustomTextSet]
  //         case KeyReqImplicationsPatch    => state.unpickle[ReqImplicationsPatch]
  //         case KeyReqTagsPatch            => state.unpickle[ReqTagsPatch]
  //         case KeyReqsDelete              => state.unpickle[ReqsDelete]
  //         case KeySavedViewCreate         => state.unpickle[SavedViewCreate]
  //         case KeySavedViewDefaultSet     => state.unpickle[SavedViewDefaultSet]
  //         case KeySavedViewDelete         => state.unpickle[SavedViewDelete]
  //         case KeySavedViewUpdate         => state.unpickle[SavedViewUpdate]
  //         case KeyTagDelete               => state.unpickle[TagDelete]
  //         case KeyTagGroupCreate          => state.unpickle[TagGroupCreate]
  //         case KeyTagGroupUpdate          => state.unpickle[TagGroupUpdate]
  //         case KeyTagRestore              => state.unpickle[TagRestore]
  //         case KeyUseCaseCreate           => state.unpickle[UseCaseCreate]
  //         case KeyUseCaseStepCreate       => state.unpickle[UseCaseStepCreate]
  //         case KeyUseCaseStepDelete       => state.unpickle[UseCaseStepDelete]
  //         case KeyUseCaseStepRestore      => state.unpickle[UseCaseStepRestore]
  //         case KeyUseCaseStepShiftLeft    => state.unpickle[UseCaseStepShiftLeft]
  //         case KeyUseCaseStepShiftRight   => state.unpickle[UseCaseStepShiftRight]
  //         case KeyUseCaseStepUpdate       => state.unpickle[UseCaseStepUpdate]
  //         case KeyUseCaseTitleSet         => state.unpickle[UseCaseTitleSet]
  //       }
  //   }

  // Replaced by v1.1
  // implicit val picklerActiveEvent: Pickler[ActiveEvent] =
  //   transformPickler[ActiveEvent, Event](Event.toActiveEvent)(e => e)
}
