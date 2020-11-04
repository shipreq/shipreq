package shipreq.webapp.member.protocol.binary.v1

import shipreq.webapp.member.data.DataImplicits._
import shipreq.webapp.member.data._
import shipreq.webapp.member.event._
import shipreq.webapp.member.sort.SortMethod

/** v1.1 */
object Rev1 {
  import boopickle.DefaultBasic._
  import shipreq.webapp.base.protocol.binary.v1.BaseData._
  import BaseMemberData1._
  import BaseMemberData1.SavedViewPicklers._
  import BaseMemberData2._

  // ===================================================================================================================

  object SavedViewPicklers {
    import shipreq.webapp.member.data.savedview._

    implicit val picklerColumn: Pickler[Column] =
      new Pickler[Column] {
        import Column._
        private[this] final val KeyCode           = 0
        private[this] final val KeyCustomField    = 1
        private[this] final val KeyDeletionReason = 2
        private[this] final val KeyImplications   = 3
        private[this] final val KeyPubid          = 4
        private[this] final val KeyReqType        = 5
        private[this] final val KeyOtherTags      = 6
        private[this] final val KeyTitle          = 7
        private[this] final val KeyAllTags        = 8
        override def pickle(a: Column)(implicit state: PickleState): Unit =
          a match {
            case Code              => state.enc.writeByte(KeyCode          )
            case b: CustomField    => state.enc.writeByte(KeyCustomField   ); state.pickle(b)
            case DeletionReason    => state.enc.writeByte(KeyDeletionReason)
            case b: Implications   => state.enc.writeByte(KeyImplications  ); state.pickle(b)
            case Pubid             => state.enc.writeByte(KeyPubid         )
            case ReqType           => state.enc.writeByte(KeyReqType       )
            case OtherTags         => state.enc.writeByte(KeyOtherTags     )
            case AllTags           => state.enc.writeByte(KeyAllTags       )
            case Title             => state.enc.writeByte(KeyTitle         )
          }
        override def unpickle(implicit state: UnpickleState): Column =
          state.dec.readByte match {
            case KeyCode           => Code
            case KeyCustomField    => state.unpickle[CustomField]
            case KeyDeletionReason => DeletionReason
            case KeyImplications   => state.unpickle[Implications]
            case KeyPubid          => Pubid
            case KeyReqType        => ReqType
            case KeyOtherTags      => OtherTags
            case KeyAllTags        => AllTags
            case KeyTitle          => Title
          }
      }

    implicit val picklerColumnSortInconclusive: Pickler[Column.SortInconclusive] =
      new Pickler[Column.SortInconclusive] {
        private[this] final val KeyCode           = 0
        private[this] final val KeyCustomField    = 1
        private[this] final val KeyDeletionReason = 2
        private[this] final val KeyImplications   = 3
        private[this] final val KeyReqType        = 4
        private[this] final val KeyOtherTags      = 5
        private[this] final val KeyTitle          = 6
        private[this] final val KeyAllTags        = 7
        override def pickle(a: Column.SortInconclusive)(implicit state: PickleState): Unit =
          a match {
            case Column.Code              => state.enc.writeByte(KeyCode          )
            case b: Column.CustomField    => state.enc.writeByte(KeyCustomField   ); state.pickle(b)
            case Column.DeletionReason    => state.enc.writeByte(KeyDeletionReason)
            case b: Column.Implications   => state.enc.writeByte(KeyImplications  ); state.pickle(b)
            case Column.ReqType           => state.enc.writeByte(KeyReqType       )
            case Column.OtherTags         => state.enc.writeByte(KeyOtherTags     )
            case Column.AllTags           => state.enc.writeByte(KeyAllTags       )
            case Column.Title             => state.enc.writeByte(KeyTitle         )
          }
        override def unpickle(implicit state: UnpickleState): Column.SortInconclusive =
          state.dec.readByte match {
            case KeyCode           => Column.Code
            case KeyCustomField    => state.unpickle[Column.CustomField]
            case KeyDeletionReason => Column.DeletionReason
            case KeyImplications   => state.unpickle[Column.Implications]
            case KeyReqType        => Column.ReqType
            case KeyOtherTags      => Column.OtherTags
            case KeyAllTags        => Column.AllTags
            case KeyTitle          => Column.Title
          }
      }

    implicit val picklerColumnSortInconclusiveHasBlanks: Pickler[Column.SortInconclusiveHasBlanks] =
      new Pickler[Column.SortInconclusiveHasBlanks] {
        private[this] final val KeyCode           = 0
        private[this] final val KeyCustomField    = 1
        private[this] final val KeyDeletionReason = 2
        private[this] final val KeyImplications   = 3
        private[this] final val KeyOtherTags      = 4
        private[this] final val KeyTitle          = 5
        private[this] final val KeyAllTags        = 6
        override def pickle(a: Column.SortInconclusiveHasBlanks)(implicit state: PickleState): Unit =
          a match {
            case Column.Code              => state.enc.writeByte(KeyCode          )
            case b: Column.CustomField    => state.enc.writeByte(KeyCustomField   ); state.pickle(b)
            case Column.DeletionReason    => state.enc.writeByte(KeyDeletionReason)
            case b: Column.Implications   => state.enc.writeByte(KeyImplications  ); state.pickle(b)
            case Column.OtherTags         => state.enc.writeByte(KeyOtherTags     )
            case Column.AllTags           => state.enc.writeByte(KeyAllTags       )
            case Column.Title             => state.enc.writeByte(KeyTitle         )
          }
        override def unpickle(implicit state: UnpickleState): Column.SortInconclusiveHasBlanks =
          state.dec.readByte match {
            case KeyCode           => Column.Code
            case KeyCustomField    => state.unpickle[Column.CustomField]
            case KeyDeletionReason => Column.DeletionReason
            case KeyImplications   => state.unpickle[Column.Implications]
            case KeyOtherTags      => Column.OtherTags
            case KeyAllTags        => Column.AllTags
            case KeyTitle          => Column.Title
          }
      }

    implicit val pickleColumnSIs: Pickler[Vector[Column.SortInconclusive]] =
      iterablePickler

    implicit val pickleColumnNEV: Pickler[NonEmptyVector[Column]] =
      pickleNEV

    implicit val picklerSortCriterionInconclusiveCB: Pickler[SortCriterion.InconclusiveCB] =
      new Pickler[SortCriterion.InconclusiveCB] {
        override def pickle(a: SortCriterion.InconclusiveCB)(implicit state: PickleState): Unit = {
          state.pickle(a.column)
          state.pickle(a.method)
        }
        override def unpickle(implicit state: UnpickleState): SortCriterion.InconclusiveCB = {
          val column = state.unpickle[Column.SortInconclusiveHasBlanks]
          val method = state.unpickle[SortMethod.ConsiderBlanks]
          SortCriterion.InconclusiveCB(column, method)
        }
      }

    implicit val picklerSortCriterionInconclusive: Pickler[SortCriterion.Inconclusive] =
      new Pickler[SortCriterion.Inconclusive] {
        private[this] final val KeyInconclusiveCB = 0
        private[this] final val KeyInconclusiveIB = 1
        override def pickle(a: SortCriterion.Inconclusive)(implicit state: PickleState): Unit =
          a match {
            case b: SortCriterion.InconclusiveCB => state.enc.writeByte(KeyInconclusiveCB); state.pickle(b)
            case b: SortCriterion.InconclusiveIB => state.enc.writeByte(KeyInconclusiveIB); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): SortCriterion.Inconclusive =
          state.dec.readByte match {
            case KeyInconclusiveCB => state.unpickle[SortCriterion.InconclusiveCB]
            case KeyInconclusiveIB => state.unpickle[SortCriterion.InconclusiveIB]
          }
      }

    implicit val picklerSortCriterion: Pickler[SortCriterion] =
      new Pickler[SortCriterion] {
        import SortCriterion._
        private[this] final val KeyConclusive     = 0
        private[this] final val KeyInconclusiveCB = 1
        private[this] final val KeyInconclusiveIB = 2
        override def pickle(a: SortCriterion)(implicit state: PickleState): Unit =
          a match {
            case b: Conclusive     => state.enc.writeByte(KeyConclusive    ); state.pickle(b)
            case b: InconclusiveCB => state.enc.writeByte(KeyInconclusiveCB); state.pickle(b)
            case b: InconclusiveIB => state.enc.writeByte(KeyInconclusiveIB); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): SortCriterion =
          state.dec.readByte match {
            case KeyConclusive     => state.unpickle[Conclusive]
            case KeyInconclusiveCB => state.unpickle[InconclusiveCB]
            case KeyInconclusiveIB => state.unpickle[InconclusiveIB]
          }
      }

    implicit val pickleSortCriterionIs: Pickler[Vector[SortCriterion.Inconclusive]] =
      iterablePickler

    implicit val picklerSortCriteria: Pickler[SortCriteria] =
      new Pickler[SortCriteria] {
        override def pickle(a: SortCriteria)(implicit state: PickleState): Unit = {
          state.pickle(a.init)
          state.pickle(a.last)
        }
        override def unpickle(implicit state: UnpickleState): SortCriteria = {
          val init = state.unpickle[Vector[SortCriterion.Inconclusive]]
          val last = state.unpickle[SortCriterion.Conclusive]
          SortCriteria(init, last)
        }
      }

    implicit val picklerImpGraphConfigGraphDir: Pickler[ImpGraphConfig.GraphDir] =
      new Pickler[ImpGraphConfig.GraphDir] {
        import ImpGraphConfig.GraphDir
        private[this] final val KeyBottomToTop = 'B'
        private[this] final val KeyLeftToRight = 'L'
        private[this] final val KeyRightToLeft = 'R'
        private[this] final val KeyTopToBottom = 'T'
        override def pickle(a: GraphDir)(implicit state: PickleState): Unit =
          a match {
            case GraphDir.BottomToTop => state.enc.writeByte(KeyBottomToTop)
            case GraphDir.LeftToRight => state.enc.writeByte(KeyLeftToRight)
            case GraphDir.RightToLeft => state.enc.writeByte(KeyRightToLeft)
            case GraphDir.TopToBottom => state.enc.writeByte(KeyTopToBottom)
          }
        override def unpickle(implicit state: UnpickleState): GraphDir =
          state.dec.readByte match {
            case KeyBottomToTop => GraphDir.BottomToTop
            case KeyLeftToRight => GraphDir.LeftToRight
            case KeyRightToLeft => GraphDir.RightToLeft
            case KeyTopToBottom => GraphDir.TopToBottom
          }
      }

    implicit val picklerImpGraphConfigColoursByTag: Pickler[ImpGraphConfig.Colours.ByTag] =
      transformPickler(ImpGraphConfig.Colours.ByTag.apply)(_.tagGroupId)

    implicit val picklerImpGraphConfigColours: Pickler[ImpGraphConfig.Colours] =
      new Pickler[ImpGraphConfig.Colours] {
        import ImpGraphConfig.Colours
        private[this] final val KeyByReqType = 0
        private[this] final val KeyByTag     = 1
        override def pickle(a: Colours)(implicit state: PickleState): Unit =
          a match {
            case Colours.ByReqType => state.enc.writeByte(KeyByReqType)
            case b: Colours.ByTag  => state.enc.writeByte(KeyByTag    ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Colours =
          state.dec.readByte match {
            case KeyByReqType => Colours.ByReqType
            case KeyByTag     => state.unpickle[Colours.ByTag]
          }
      }

    implicit val picklerImpGraphConfigLabelFormat: Pickler[ImpGraphConfig.LabelFormat] =
      new Pickler[ImpGraphConfig.LabelFormat] {
        private[this] final val KeyPubid         = 0
        private[this] final val KeyPubidAndTitle = 1
        override def pickle(a: ImpGraphConfig.LabelFormat)(implicit state: PickleState): Unit =
          a match {
            case ImpGraphConfig.LabelFormat.Pubid         => state.enc.writeByte(KeyPubid        )
            case ImpGraphConfig.LabelFormat.PubidAndTitle => state.enc.writeByte(KeyPubidAndTitle)
          }
        override def unpickle(implicit state: UnpickleState): ImpGraphConfig.LabelFormat =
          state.dec.readByte match {
            case KeyPubid         => ImpGraphConfig.LabelFormat.Pubid
            case KeyPubidAndTitle => ImpGraphConfig.LabelFormat.PubidAndTitle
          }
      }

    implicit val picklerImpGraphConfig: Pickler[ImpGraphConfig] =
      new Pickler[ImpGraphConfig] {
        override def pickle(a: ImpGraphConfig)(implicit state: PickleState): Unit = {
          state.pickle(a.graphDir)
          state.pickle(a.labelFormat)
          state.pickle(a.colours)
        }
        override def unpickle(implicit state: UnpickleState): ImpGraphConfig = {
          val graphDir    = state.unpickle[ImpGraphConfig.GraphDir]
          val labelFormat = state.unpickle[ImpGraphConfig.LabelFormat]
          val colours     = state.unpickle[ImpGraphConfig.Colours]
          ImpGraphConfig(graphDir, labelFormat, colours)
        }
      }
  }


  // ===================================================================================================================

  implicit lazy val picklerColour: Pickler[Colour] =
    transformPickler(Colour.force)(_.value)

  implicit lazy val picklerApplicableTag: Pickler[ApplicableTag] =
    new Pickler[ApplicableTag] {
      override def pickle(a: ApplicableTag)(implicit state: PickleState): Unit = {
        writeVersion(1)
        state.pickle(a.id)
        state.pickle(a.key)
        state.pickle(a.desc)
        state.pickle(a.colour)
        state.pickle(a.applicableReqTypes)
        state.pickle(a.live)
      }
      override def unpickle(implicit state: UnpickleState): ApplicableTag =
        readByVersion(1) {

          // v1.0
          case 0 =>
            val id   = state.unpickle[ApplicableTagId]
            val name = state.unpickle[String]
            val desc = state.unpickle[Option[String]]
            val key  = state.unpickle[HashRefKey]
            val live = state.unpickle[Live]
            ApplicableTag.v1(id, name, desc, key, live)

          // v1.1
          case 1 =>
            val id       = state.unpickle[ApplicableTagId]
            val key      = state.unpickle[HashRefKey]
            val desc     = state.unpickle[Option[String]]
            val colour   = state.unpickle[Option[Colour]]
            val reqTypes = state.unpickle[ApplicableReqTypes]
            val live     = state.unpickle[Live]
            ApplicableTag(id, key, desc, colour, reqTypes, live)
        }
    }

  implicit lazy val picklerTag: Pickler[Tag] =
    new Pickler[Tag] {
      private[this] final val KeyApplicableTag = 'a'
      private[this] final val KeyTagGroup      = 'g'
      override def pickle(a: Tag)(implicit state: PickleState): Unit =
        a match {
          case b: ApplicableTag => state.enc.writeByte(KeyApplicableTag); state.pickle(b)
          case b: TagGroup      => state.enc.writeByte(KeyTagGroup     ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): Tag =
        state.dec.readByte match {
          case KeyApplicableTag => state.unpickle[ApplicableTag]
          case KeyTagGroup      => state.unpickle[TagGroup]
        }
    }

  implicit lazy val picklerTagInTree: Pickler[TagInTree] =
    new Pickler[TagInTree] {
      override def pickle(a: TagInTree)(implicit state: PickleState): Unit = {
        state.pickle(a.tag)
        state.pickle(a.children)
      }
      override def unpickle(implicit state: UnpickleState): TagInTree = {
        val tag      = state.unpickle[Tag]
        val children = state.unpickle[TagInTree.Children]
        TagInTree(tag, children)
      }
    }

  implicit lazy val picklerTags: Pickler[Tags] =
    transformPickler(Tags.apply)(_.tree)

  implicit lazy val picklerTagTree: Pickler[TagTree] =
    pickleIMap(TagTree.empty)

  implicit def picklerFieldReqTypeRulesDefaultTo[D: Pickler]: Pickler[FieldReqTypeRules.Resolution.DefaultTo[D]] =
    transformPickler(FieldReqTypeRules.Resolution.DefaultTo.apply[D])(_.default)

  implicit def picklerFieldReqTypeRulesResolution[D](implicit p1: Pickler[FieldReqTypeRules.Resolution.DefaultTo[D]]): Pickler[FieldReqTypeRules.Resolution[D]] =
    new Pickler[FieldReqTypeRules.Resolution[D]] {
      private[this] final val KeyNotApplicable = 0
      private[this] final val KeyOptional      = 1
      private[this] final val KeyMandatory     = 2
      private[this] final val KeyDefaultTo     = 3
      override def pickle(a: FieldReqTypeRules.Resolution[D])(implicit state: PickleState): Unit =
        a match {
          case FieldReqTypeRules.Resolution.NotApplicable    => state.enc.writeByte(KeyNotApplicable)
          case FieldReqTypeRules.Resolution.Optional         => state.enc.writeByte(KeyOptional     )
          case FieldReqTypeRules.Resolution.Mandatory        => state.enc.writeByte(KeyMandatory    )
          case b: FieldReqTypeRules.Resolution.DefaultTo[D]  => state.enc.writeByte(KeyDefaultTo    ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): FieldReqTypeRules.Resolution[D] =
        state.dec.readByte match {
          case KeyNotApplicable => FieldReqTypeRules.Resolution.NotApplicable
          case KeyOptional      => FieldReqTypeRules.Resolution.Optional
          case KeyMandatory     => FieldReqTypeRules.Resolution.Mandatory
          case KeyDefaultTo     => state.unpickle[FieldReqTypeRules.Resolution.DefaultTo[D]]
        }
    }

  implicit def picklerFieldReqTypeRules[D: Pickler]: Pickler[FieldReqTypeRules[D]] =
    new Pickler[FieldReqTypeRules[D]] {
      override def pickle(a: FieldReqTypeRules[D])(implicit state: PickleState): Unit = {
        state.pickle(a.perReqType)
        state.pickle(a.otherwise)
      }
      override def unpickle(implicit state: UnpickleState): FieldReqTypeRules[D] = {
        val perReqType = state.unpickle[Map[ReqTypeId, FieldReqTypeRules.Resolution[D]]]
        val otherwise  = state.unpickle[FieldReqTypeRules.Resolution[D]]
        FieldReqTypeRules(perReqType, otherwise)
      }
    }

  implicit lazy val picklerCustomFieldImplication: Pickler[CustomField.Implication] =
    new Pickler[CustomField.Implication] {
      override def pickle(a: CustomField.Implication)(implicit state: PickleState): Unit = {
        writeVersion(1)
        state.pickle(a.id)
        state.pickle(a.reqTypeId)
        state.pickle(a.fieldReqTypeRules)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): CustomField.Implication =
        readByVersion(1) {

          // v1.0
          case 0 =>
            val id             = state.unpickle[CustomField.Implication.Id]
            val reqTypeId      = state.unpickle[ReqTypeId]
            val mandatory      = state.unpickle[Mandatory]
            val reqTypes       = state.unpickle[ApplicableReqTypes]
            val liveExplicitly = state.unpickle[Live]
            CustomField.Implication.v1(id, reqTypeId, mandatory, reqTypes, liveExplicitly)

          // v1.1
          case 1 =>
            val id             = state.unpickle[CustomField.Implication.Id]
            val reqTypeId      = state.unpickle[ReqTypeId]
            val reqTypes       = state.unpickle[FieldReqTypeRules.ForImpField]
            val liveExplicitly = state.unpickle[Live]
            CustomField.Implication(id, reqTypeId, reqTypes, liveExplicitly)
        }
    }

  implicit lazy val picklerCustomFieldText: Pickler[CustomField.Text] =
    new Pickler[CustomField.Text] {
      override def pickle(a: CustomField.Text)(implicit state: PickleState): Unit = {
        writeVersion(1)
        state.pickle(a.id)
        state.pickle(a.name)
        state.pickle(a.fieldReqTypeRules)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): CustomField.Text =
        readByVersion(1) {

          // v1.0
          case 0 =>
            val id             = state.unpickle[CustomField.Text.Id]
            val name           = state.unpickle[String]
            val key            = state.unpickle[String]
            val mandatory      = state.unpickle[Mandatory]
            val reqTypes       = state.unpickle[ApplicableReqTypes]
            val liveExplicitly = state.unpickle[Live]
            CustomField.Text.v1(id, name, key, mandatory, reqTypes, liveExplicitly)

          // v1.1
          case 1 =>
            val id             = state.unpickle[CustomField.Text.Id]
            val name           = state.unpickle[String]
            val reqTypes       = state.unpickle[FieldReqTypeRules.ForTextField]
            val liveExplicitly = state.unpickle[Live]
            CustomField.Text(id, name, reqTypes, liveExplicitly)
        }
    }

  implicit lazy val picklerFieldId: Pickler[FieldId] =
    new Pickler[FieldId] {
      private[this] final val KeyCustomImplication       = 'i'
      private[this] final val KeyCustomTag               = 't'
      private[this] final val KeyCustomText              = 'x'
      private[this] final val KeyStaticExceptionStepTree = 'E'
      private[this] final val KeyStaticImplicationGraph  = 'I'
      private[this] final val KeyStaticNormalAltStepTree = 'N'
      private[this] final val KeyStaticStepGraph         = 'G'
      private[this] final val KeyStaticOtherTags         = '#'
      private[this] final val KeyStaticAllTags           = 'T'
      override def pickle(a: FieldId)(implicit state: PickleState): Unit =
        a match {
          case b: CustomField.Implication.Id => state.enc.writeByte(KeyCustomImplication          ); state.pickle(b)
          case b: CustomField.Tag        .Id => state.enc.writeByte(KeyCustomTag                  ); state.pickle(b)
          case b: CustomField.Text       .Id => state.enc.writeByte(KeyCustomText                 ); state.pickle(b)
          case StaticField.ExceptionStepTree => state.enc.writeByte(KeyStaticExceptionStepTree    )
          case StaticField.ImplicationGraph  => state.enc.writeByte(KeyStaticImplicationGraph     )
          case StaticField.NormalAltStepTree => state.enc.writeByte(KeyStaticNormalAltStepTree    )
          case StaticField.StepGraph         => state.enc.writeByte(KeyStaticStepGraph            )
          case StaticField.OtherTags         => state.enc.writeByte(KeyStaticOtherTags            )
          case StaticField.AllTags           => state.enc.writeByte(KeyStaticAllTags              )
        }
      override def unpickle(implicit state: UnpickleState): FieldId =
        state.dec.readByte match {
          case KeyCustomImplication       => state.unpickle[CustomField.Implication.Id]
          case KeyCustomTag               => state.unpickle[CustomField.Tag        .Id]
          case KeyCustomText              => state.unpickle[CustomField.Text       .Id]
          case KeyStaticExceptionStepTree => StaticField.ExceptionStepTree
          case KeyStaticImplicationGraph  => StaticField.ImplicationGraph
          case KeyStaticNormalAltStepTree => StaticField.NormalAltStepTree
          case KeyStaticStepGraph         => StaticField.StepGraph
          case KeyStaticOtherTags         => StaticField.OtherTags
          case KeyStaticAllTags           => StaticField.AllTags
        }
    }

  implicit lazy val picklerStaticField: Pickler[StaticField] =
    new Pickler[StaticField] {
      import StaticField._
      private[this] final val KeyExceptionStepTree = 'e'
      private[this] final val KeyImplicationGraph  = 'i'
      private[this] final val KeyNormalAltStepTree = 'n'
      private[this] final val KeyStepGraph         = 'g'
      private[this] final val KeyOtherTags         = '#'
      private[this] final val KeyAllTags           = 'T'
      override def pickle(a: StaticField)(implicit state: PickleState): Unit =
        a match {
          case ExceptionStepTree => state.enc.writeByte(KeyExceptionStepTree)
          case ImplicationGraph  => state.enc.writeByte(KeyImplicationGraph )
          case NormalAltStepTree => state.enc.writeByte(KeyNormalAltStepTree)
          case StepGraph         => state.enc.writeByte(KeyStepGraph        )
          case OtherTags         => state.enc.writeByte(KeyOtherTags        )
          case AllTags           => state.enc.writeByte(KeyAllTags          )
        }
      override def unpickle(implicit state: UnpickleState): StaticField =
        state.dec.readByte match {
          case KeyExceptionStepTree => ExceptionStepTree
          case KeyImplicationGraph  => ImplicationGraph
          case KeyNormalAltStepTree => NormalAltStepTree
          case KeyStepGraph         => StepGraph
          case KeyOtherTags         => OtherTags
          case KeyAllTags           => AllTags
        }
    }

  implicit lazy val picklerStaticFieldOptional: Pickler[StaticField.Optional] =
    picklerStaticField.narrow

  implicit lazy val picklerStaticFieldType: Pickler[StaticFieldType] =
    new Pickler[StaticFieldType] {
      private[this] final val KeyImplicationGraph = 'i'
      private[this] final val KeyUseCaseSteps     = 't'
      private[this] final val KeyUseCaseStepGraph = 'g'
      private[this] final val KeyTag              = '#'
      override def pickle(a: StaticFieldType)(implicit state: PickleState): Unit =
        a match {
          case StaticFieldType.ImplicationGraph => state.enc.writeByte(KeyImplicationGraph)
          case StaticFieldType.UseCaseSteps     => state.enc.writeByte(KeyUseCaseSteps    )
          case StaticFieldType.UseCaseStepGraph => state.enc.writeByte(KeyUseCaseStepGraph)
          case StaticFieldType.Tag              => state.enc.writeByte(KeyTag             )
        }
      override def unpickle(implicit state: UnpickleState): StaticFieldType =
        state.dec.readByte match {
          case KeyImplicationGraph => StaticFieldType.ImplicationGraph
          case KeyUseCaseSteps     => StaticFieldType.UseCaseSteps
          case KeyUseCaseStepGraph => StaticFieldType.UseCaseStepGraph
          case KeyTag              => StaticFieldType.Tag
        }
    }

  implicit lazy val picklerFieldType: Pickler[FieldType] =
    new Pickler[FieldType] {
      private[this] final val KeyImplication      = 'i'
      private[this] final val KeyImplicationGraph = 'I'
      private[this] final val KeyUseCaseStepGraph = 'G'
      private[this] final val KeyUseCaseSteps     = 'T'
      private[this] final val KeyCustomTag        = 't'
      private[this] final val KeyText             = 'x'
      private[this] final val KeyStaticTag        = '#'
      override def pickle(a: FieldType)(implicit state: PickleState): Unit =
        a match {
          case CustomFieldType.Implication      => state.enc.writeByte(KeyImplication     )
          case StaticFieldType.ImplicationGraph => state.enc.writeByte(KeyImplicationGraph)
          case StaticFieldType.UseCaseSteps     => state.enc.writeByte(KeyUseCaseSteps    )
          case StaticFieldType.UseCaseStepGraph => state.enc.writeByte(KeyUseCaseStepGraph)
          case CustomFieldType.Text             => state.enc.writeByte(KeyText            )
          case CustomFieldType.Tag              => state.enc.writeByte(KeyCustomTag       )
          case StaticFieldType.Tag              => state.enc.writeByte(KeyStaticTag       )
        }
      override def unpickle(implicit state: UnpickleState): FieldType =
        state.dec.readByte match {
          case KeyImplication      => CustomFieldType.Implication
          case KeyImplicationGraph => StaticFieldType.ImplicationGraph
          case KeyUseCaseSteps     => StaticFieldType.UseCaseSteps
          case KeyUseCaseStepGraph => StaticFieldType.UseCaseStepGraph
          case KeyText             => CustomFieldType.Text
          case KeyCustomTag        => CustomFieldType.Tag
          case KeyStaticTag        => StaticFieldType.Tag
        }
    }

  implicit lazy val picklerCustomReqType: Pickler[CustomReqType] =
    new Pickler[CustomReqType] {
      private[this] implicit val picklerSetMnemonics: Pickler[Set[ReqType.Mnemonic]] = iterablePickler
      override def pickle(a: CustomReqType)(implicit state: PickleState): Unit = {
        writeVersion(1)
        state.pickle(a.id)
        state.pickle(a.mnemonic)
        state.pickle(a.oldMnemonics)
        state.pickle(a.name)
        state.pickle(a.description)
        state.pickle(a.implication)
        state.pickle(a.live)
      }
      override def unpickle(implicit state: UnpickleState): CustomReqType =
        readByVersion(1) {

          // v1.0
          case 0 =>
            val id           = state.unpickle[CustomReqTypeId]
            val mnemonic     = state.unpickle[ReqType.Mnemonic]
            val oldMnemonics = state.unpickle[Set[ReqType.Mnemonic]]
            val name         = state.unpickle[String]
            val imp          = state.unpickle[Mandatory]
            val live         = state.unpickle[Live]
            CustomReqType.v1(id, mnemonic, oldMnemonics, name, imp, live)

          // v1.1
          case 1 =>
            val id           = state.unpickle[CustomReqTypeId]
            val mnemonic     = state.unpickle[ReqType.Mnemonic]
            val oldMnemonics = state.unpickle[Set[ReqType.Mnemonic]]
            val name         = state.unpickle[String]
            val desc         = state.unpickle[Option[String]]
            val imp          = state.unpickle[Mandatory]
            val live         = state.unpickle[Live]
            CustomReqType(id, mnemonic, oldMnemonics, name, desc, imp, live)
        }
    }

  implicit lazy val picklerReqTypesCustom: Pickler[ReqTypes.Custom] =
    pickleIMapD[CustomReqTypeId, CustomReqType]

  implicit lazy val picklerReqTypes: Pickler[ReqTypes] =
    transformPickler(ReqTypes.apply)(_.custom)

  // ===================================================================================================================

  implicit lazy val pickleApplicableTagGD: Pickler[ApplicableTagGD.NonEmptyValues] = {
    import ApplicableTagGD._

    implicit val picklerValueForApplicableReqTypes = transformPickler(ValueForApplicableReqTypes.apply)(_.value)
    implicit val picklerValueForChildren           = transformPickler(ValueForChildren          .apply)(_.value)
    implicit val picklerValueForColour             = transformPickler(ValueForColour            .apply)(_.value)
    implicit val picklerValueForDesc               = transformPickler(ValueForDesc              .apply)(_.value)
    implicit val picklerValueForKey                = transformPickler(ValueForKey               .apply)(_.value)
    implicit val picklerValueForParents            = transformPickler(ValueForParents           .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyForApplicableReqTypes = 'r'
        private[this] final val KeyForChildren           = 'C'
        private[this] final val KeyForColour             = 'c'
        private[this] final val KeyForDesc               = 'd'
        private[this] final val KeyForKey                = '#'
        private[this] final val KeyForParents            = 'P'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForApplicableReqTypes => state.enc.writeByte(KeyForApplicableReqTypes); state.pickle(b)
            case b: ValueForChildren           => state.enc.writeByte(KeyForChildren          ); state.pickle(b)
            case b: ValueForColour             => state.enc.writeByte(KeyForColour            ); state.pickle(b)
            case b: ValueForDesc               => state.enc.writeByte(KeyForDesc              ); state.pickle(b)
            case b: ValueForKey                => state.enc.writeByte(KeyForKey               ); state.pickle(b)
            case b: ValueForParents            => state.enc.writeByte(KeyForParents           ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyForApplicableReqTypes => state.unpickle[ValueForApplicableReqTypes]
            case KeyForChildren           => state.unpickle[ValueForChildren]
            case KeyForColour             => state.unpickle[ValueForColour]
            case KeyForDesc               => state.unpickle[ValueForDesc]
            case KeyForKey                => state.unpickle[ValueForKey]
            case KeyForParents            => state.unpickle[ValueForParents]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit lazy val pickleCustomImpFieldGD: Pickler[CustomImpFieldGD.NonEmptyValues] = {
    import CustomImpFieldGD._

    implicit val picklerValueForFieldReqTypeRules = transformPickler(ValueForFieldReqTypeRules.apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyReqTypes = 'R'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForFieldReqTypeRules => state.enc.writeByte(KeyReqTypes); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyReqTypes => state.unpickle[ValueForFieldReqTypeRules]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit lazy val pickleCustomReqTypeGD: Pickler[CustomReqTypeGD.NonEmptyValues] = {
    import CustomReqTypeGD._

    implicit val picklerValueForDescription = transformPickler(ValueForDescription.apply)(_.value)
    implicit val picklerValueForImplication = transformPickler(ValueForImplication.apply)(_.value)
    implicit val picklerValueForMnemonic    = transformPickler(ValueForMnemonic   .apply)(_.value)
    implicit val picklerValueForName        = transformPickler(ValueForName       .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyImplication = 'I'
        private[this] final val KeyMnemonic    = 'M'
        private[this] final val KeyName        = 'N'
        private[this] final val KeyDescription = 'D'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForDescription => state.enc.writeByte(KeyDescription); state.pickle(b)
            case b: ValueForImplication => state.enc.writeByte(KeyImplication); state.pickle(b)
            case b: ValueForMnemonic    => state.enc.writeByte(KeyMnemonic   ); state.pickle(b)
            case b: ValueForName        => state.enc.writeByte(KeyName       ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyDescription => state.unpickle[ValueForDescription]
            case KeyImplication => state.unpickle[ValueForImplication]
            case KeyMnemonic    => state.unpickle[ValueForMnemonic]
            case KeyName        => state.unpickle[ValueForName]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit lazy val pickleCustomTextFieldGD: Pickler[CustomTextFieldGD.NonEmptyValues] = {
    import CustomTextFieldGD._

    implicit val picklerValueForName              = transformPickler(ValueForName             .apply)(_.value)
    implicit val picklerValueForFieldReqTypeRules = transformPickler(ValueForFieldReqTypeRules.apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyName      = 'N'
        private[this] final val KeyReqTypes  = 'R'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForName              => state.enc.writeByte(KeyName    ); state.pickle(b)
            case b: ValueForFieldReqTypeRules => state.enc.writeByte(KeyReqTypes); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyName      => state.unpickle[ValueForName]
            case KeyReqTypes  => state.unpickle[ValueForFieldReqTypeRules]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  private[v1] implicit lazy val picklerEventApplicableTagCreate: Pickler[Event.ApplicableTagCreate] =
    new Pickler[Event.ApplicableTagCreate] {
      override def pickle(a: Event.ApplicableTagCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.ApplicableTagCreate = {
        val id = state.unpickle[ApplicableTagId]
        val vs = state.unpickle[ApplicableTagGD.NonEmptyValues]
        Event.ApplicableTagCreate(id, vs)
      }
    }

  private[v1] implicit lazy val picklerEventApplicableTagUpdate: Pickler[Event.ApplicableTagUpdate] =
    new Pickler[Event.ApplicableTagUpdate] {
      override def pickle(a: Event.ApplicableTagUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.ApplicableTagUpdate = {
        val id = state.unpickle[ApplicableTagId]
        val vs = state.unpickle[ApplicableTagGD.NonEmptyValues]
        Event.ApplicableTagUpdate(id, vs)
      }
    }

  private[v1] implicit lazy val picklerEventFieldCustomTextCreate: Pickler[Event.FieldCustomTextCreate] =
    new Pickler[Event.FieldCustomTextCreate] {
      override def pickle(a: Event.FieldCustomTextCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomTextCreate = {
        val id = state.unpickle[CustomField.Text.Id]
        val vs = state.unpickle[CustomTextFieldGD.NonEmptyValues]
        Event.FieldCustomTextCreate(id, vs)
      }
    }

  private[v1] implicit lazy val picklerEventFieldCustomTextUpdate: Pickler[Event.FieldCustomTextUpdate] =
    new Pickler[Event.FieldCustomTextUpdate] {
      override def pickle(a: Event.FieldCustomTextUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomTextUpdate = {
        val id = state.unpickle[CustomField.Text.Id]
        val vs = state.unpickle[CustomTextFieldGD.NonEmptyValues]
        Event.FieldCustomTextUpdate(id, vs)
      }
    }

  private[v1] implicit lazy val picklerEventFieldCustomImpCreate: Pickler[Event.FieldCustomImpCreate] =
    new Pickler[Event.FieldCustomImpCreate] {
      override def pickle(a: Event.FieldCustomImpCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.reqTypeId)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomImpCreate = {
        val id        = state.unpickle[CustomField.Implication.Id]
        val reqTypeId = state.unpickle[ReqTypeId]
        val vs        = state.unpickle[CustomImpFieldGD.NonEmptyValues]
        Event.FieldCustomImpCreate(id, reqTypeId, vs)
      }
    }

  private[v1] implicit lazy val picklerEventFieldCustomImpUpdate: Pickler[Event.FieldCustomImpUpdate] =
    new Pickler[Event.FieldCustomImpUpdate] {
      override def pickle(a: Event.FieldCustomImpUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomImpUpdate = {
        val id = state.unpickle[CustomField.Implication.Id]
        val vs = state.unpickle[CustomImpFieldGD.NonEmptyValues]
        Event.FieldCustomImpUpdate(id, vs)
      }
    }

  private[v1] implicit lazy val picklerEventCustomReqTypeDeleteHard: Pickler[Event.CustomReqTypeDeleteHard] =
    transformPickler(Event.CustomReqTypeDeleteHard.apply)(_.id)

  private[v1] implicit lazy val picklerEventCustomReqTypeDeleteSoft: Pickler[Event.CustomReqTypeDeleteSoft] =
    transformPickler(Event.CustomReqTypeDeleteSoft.apply)(_.id)

  private[v1] implicit lazy val picklerEventCustomReqTypeCreate: Pickler[Event.CustomReqTypeCreate] =
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

  private[v1] implicit lazy val picklerEventCustomReqTypeUpdate: Pickler[Event.CustomReqTypeUpdate] =
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
}
