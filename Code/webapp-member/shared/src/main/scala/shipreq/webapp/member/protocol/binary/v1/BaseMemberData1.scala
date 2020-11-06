package shipreq.webapp.member.protocol.binary.v1

import boopickle.DefaultBasic._
import nyaya.util.Multimap
import shipreq.base.util._
import shipreq.webapp.base.util._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.issue.IssueCategory
import shipreq.webapp.member.project.sort.SortMethod

/** This is the minimum set of codecs required for event codecs.
  *
  * Events (and their dependencies) are expected to be extremely stable and only change very, very rarely if ever.
  */
object BaseMemberData1 {
  import shipreq.webapp.base.protocol.binary.v1.BaseData._

  // ===================================================================================================================
  // Polymorphic definitions
  // (non-implicit, "pickle" prefix)

  def pickleIMapD[K: UnivEq, V: Pickler](implicit d: DataIdAux[V, K]): Pickler[IMap[K, V]] =
    pickleIMap(d.emptyIMap)

  // ===================================================================================================================
  // Concrete picklers for base data type
  // (implicit lazy vals, "pickler" prefix)

  object SavedViewPicklers {
    import shipreq.webapp.member.project.data.savedview._

    implicit val picklerColumnImplications: Pickler[Column.Implications] =
      transformPickler(Column.Implications.apply)(_.dir)

    implicit val picklerColumnCustomField: Pickler[Column.CustomField] =
      transformPickler(Column.CustomField.apply)(_.id)

    implicit val picklerColumnSortConclusive: Pickler[Column.SortConclusive] =
      new Pickler[Column.SortConclusive] {
        private[this] final val KeyPubid = 0
        override def pickle(a: Column.SortConclusive)(implicit state: PickleState): Unit =
          a match {
            case Column.Pubid => state.enc.writeByte(KeyPubid)
          }
        override def unpickle(implicit state: UnpickleState): Column.SortConclusive =
          state.dec.readByte match {
            case KeyPubid => Column.Pubid
          }
      }

    implicit val picklerColumnSortInconclusiveNoBlanks: Pickler[Column.SortInconclusiveNoBlanks] =
      new Pickler[Column.SortInconclusiveNoBlanks] {
        private[this] final val KeyReqType = 0
        override def pickle(a: Column.SortInconclusiveNoBlanks)(implicit state: PickleState): Unit =
          a match {
            case Column.ReqType => state.enc.writeByte(KeyReqType)
          }
        override def unpickle(implicit state: UnpickleState): Column.SortInconclusiveNoBlanks =
          state.dec.readByte match {
            case KeyReqType => Column.ReqType
          }
      }

    implicit val picklerSortMethod: Pickler[SortMethod] =
      new Pickler[SortMethod] {
        import SortMethod._
        private[this] final val KeyAsc            = 0
        private[this] final val KeyAscThenBlanks  = 1
        private[this] final val KeyBlanksThenAsc  = 2
        private[this] final val KeyBlanksThenDesc = 3
        private[this] final val KeyDesc           = 4
        private[this] final val KeyDescThenBlanks = 5
        override def pickle(a: SortMethod)(implicit state: PickleState): Unit =
          a match {
            case Asc            => state.enc.writeByte(KeyAsc           )
            case AscThenBlanks  => state.enc.writeByte(KeyAscThenBlanks )
            case BlanksThenAsc  => state.enc.writeByte(KeyBlanksThenAsc )
            case BlanksThenDesc => state.enc.writeByte(KeyBlanksThenDesc)
            case Desc           => state.enc.writeByte(KeyDesc          )
            case DescThenBlanks => state.enc.writeByte(KeyDescThenBlanks)
          }
        override def unpickle(implicit state: UnpickleState): SortMethod =
          state.dec.readByte match {
            case KeyAsc            => Asc
            case KeyAscThenBlanks  => AscThenBlanks
            case KeyBlanksThenAsc  => BlanksThenAsc
            case KeyBlanksThenDesc => BlanksThenDesc
            case KeyDesc           => Desc
            case KeyDescThenBlanks => DescThenBlanks
          }
      }

    /*
    implicit val picklerSortMethodAscHalf: Pickler[SortMethod.AscHalf] =
      new Pickler[SortMethod.AscHalf] {
        private[this] final val KeyAsc           = 0
        private[this] final val KeyAscThenBlanks = 1
        private[this] final val KeyBlanksThenAsc = 2
        override def pickle(a: SortMethod.AscHalf)(implicit state: PickleState): Unit =
          a match {
            case SortMethod.Asc           => state.enc.writeByte(KeyAsc          )
            case SortMethod.AscThenBlanks => state.enc.writeByte(KeyAscThenBlanks)
            case SortMethod.BlanksThenAsc => state.enc.writeByte(KeyBlanksThenAsc)
          }
        override def unpickle(implicit state: UnpickleState): SortMethod.AscHalf =
          state.dec.readByte match {
            case KeyAsc           => SortMethod.Asc
            case KeyAscThenBlanks => SortMethod.AscThenBlanks
            case KeyBlanksThenAsc => SortMethod.BlanksThenAsc
          }
      }

    implicit val picklerSortMethodDescHalf: Pickler[SortMethod.DescHalf] =
      new Pickler[SortMethod.DescHalf] {
        private[this] final val KeyBlanksThenDesc = 0
        private[this] final val KeyDesc           = 1
        private[this] final val KeyDescThenBlanks = 2
        override def pickle(a: SortMethod.DescHalf)(implicit state: PickleState): Unit =
          a match {
            case SortMethod.BlanksThenDesc => state.enc.writeByte(KeyBlanksThenDesc)
            case SortMethod.Desc           => state.enc.writeByte(KeyDesc          )
            case SortMethod.DescThenBlanks => state.enc.writeByte(KeyDescThenBlanks)
          }
        override def unpickle(implicit state: UnpickleState): SortMethod.DescHalf =
          state.dec.readByte match {
            case KeyBlanksThenDesc => SortMethod.BlanksThenDesc
            case KeyDesc           => SortMethod.Desc
            case KeyDescThenBlanks => SortMethod.DescThenBlanks
          }
      }
      */

    implicit val picklerSortMethodIgnoreBlanks: Pickler[SortMethod.IgnoreBlanks] =
      new Pickler[SortMethod.IgnoreBlanks] {
        private[this] final val KeyAsc  = 0
        private[this] final val KeyDesc = 1
        override def pickle(a: SortMethod.IgnoreBlanks)(implicit state: PickleState): Unit =
          a match {
            case SortMethod.Asc  => state.enc.writeByte(KeyAsc )
            case SortMethod.Desc => state.enc.writeByte(KeyDesc)
          }
        override def unpickle(implicit state: UnpickleState): SortMethod.IgnoreBlanks =
          state.dec.readByte match {
            case KeyAsc  => SortMethod.Asc
            case KeyDesc => SortMethod.Desc
          }
      }

    implicit val picklerSortMethodConsiderBlanks: Pickler[SortMethod.ConsiderBlanks] =
      new Pickler[SortMethod.ConsiderBlanks] {
        private[this] final val KeyAscThenBlanks  = 0
        private[this] final val KeyBlanksThenAsc  = 1
        private[this] final val KeyBlanksThenDesc = 2
        private[this] final val KeyDescThenBlanks = 3
        override def pickle(a: SortMethod.ConsiderBlanks)(implicit state: PickleState): Unit =
          a match {
            case SortMethod.AscThenBlanks  => state.enc.writeByte(KeyAscThenBlanks )
            case SortMethod.BlanksThenAsc  => state.enc.writeByte(KeyBlanksThenAsc )
            case SortMethod.BlanksThenDesc => state.enc.writeByte(KeyBlanksThenDesc)
            case SortMethod.DescThenBlanks => state.enc.writeByte(KeyDescThenBlanks)
          }
        override def unpickle(implicit state: UnpickleState): SortMethod.ConsiderBlanks =
          state.dec.readByte match {
            case KeyAscThenBlanks  => SortMethod.AscThenBlanks
            case KeyBlanksThenAsc  => SortMethod.BlanksThenAsc
            case KeyBlanksThenDesc => SortMethod.BlanksThenDesc
            case KeyDescThenBlanks => SortMethod.DescThenBlanks
          }
      }

    implicit val picklerSortCriterionInconclusiveIB: Pickler[SortCriterion.InconclusiveIB] =
      new Pickler[SortCriterion.InconclusiveIB] {
        override def pickle(a: SortCriterion.InconclusiveIB)(implicit state: PickleState): Unit = {
          state.pickle(a.column)
          state.pickle(a.method)
        }
        override def unpickle(implicit state: UnpickleState): SortCriterion.InconclusiveIB = {
          val column = state.unpickle[Column.SortInconclusiveNoBlanks]
          val method = state.unpickle[SortMethod.IgnoreBlanks]
          SortCriterion.InconclusiveIB(column, method)
        }
      }

    implicit val picklerSortCriterionConclusive: Pickler[SortCriterion.Conclusive] =
      new Pickler[SortCriterion.Conclusive] {
        override def pickle(a: SortCriterion.Conclusive)(implicit state: PickleState): Unit = {
          state.pickle(a.column)
          state.pickle(a.method)
        }
        override def unpickle(implicit state: UnpickleState): SortCriterion.Conclusive = {
          val column = state.unpickle[Column.SortConclusive]
          val method = state.unpickle[SortMethod.IgnoreBlanks]
          SortCriterion.Conclusive(column, method)
        }
      }

    implicit val picklerSavedViewId: Pickler[SavedView.Id] =
      transformPickler(SavedView.Id.apply)(_.value)

    implicit val picklerSavedViewName: Pickler[SavedView.Name] =
      transformPickler(SavedView.Name.apply)(_.value)
  }

  // Note: This has been designed to be identical to ISubset[ReqTypeId] which is what it's meant to replace.
  implicit lazy val picklerApplicableReqTypes: Pickler[ApplicableReqTypes] =
    new Pickler[ApplicableReqTypes] {
      private[this] final val KeyAll  = 'a'
      private[this] final val KeyNot  = 'n'
      private[this] final val KeyOnly = 'o'

      override def pickle(a: ApplicableReqTypes)(implicit state: PickleState): Unit =
        if (a.isEmpty)
          state.enc.writeByte(KeyAll)
        else {
          state.enc.writeByte(if (a.applicability is Applicable) KeyOnly else KeyNot)
          state.pickle(a.reqTypes)
        }

      override def unpickle(implicit state: UnpickleState): ApplicableReqTypes =
        state.dec.readByte match {
          case KeyAll  => ApplicableReqTypes.empty
          case KeyNot  => ApplicableReqTypes(NotApplicable, state.unpickle[Set[ReqTypeId]])
          case KeyOnly => ApplicableReqTypes(Applicable,    state.unpickle[Set[ReqTypeId]])
        }
    }

  implicit lazy val picklerApplicableTagId: Pickler[ApplicableTagId] =
    pickleTaggedI(ApplicableTagId).reuseByUnivEq

  implicit lazy val picklerApReqCodeId: Pickler[ApReqCodeId] =
    pickleTaggedI(ApReqCodeId.apply).reuseByUnivEq

  implicit lazy val picklerApReqCodeIdAndValue: Pickler[ApReqCodeId.AndValue] =
    new Pickler[ApReqCodeId.AndValue] {
      override def pickle(a: ApReqCodeId.AndValue)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.value)
      }
      override def unpickle(implicit state: UnpickleState): ApReqCodeId.AndValue = {
        val id    = state.unpickle[ApReqCodeId]
        val value = state.unpickle[ReqCode.Value]
        ApReqCodeId.AndValue(id, value)
      }
    }

  implicit lazy val picklerCustomFieldId: Pickler[CustomFieldId] =
    new Pickler[CustomFieldId] {
      private[this] final val KeyImplication = 'i'
      private[this] final val KeyTag         = 't'
      private[this] final val KeyText        = 'x'
      override def pickle(a: CustomFieldId)(implicit state: PickleState): Unit =
        a match {
          case b: CustomField.Implication.Id => state.enc.writeByte(KeyImplication); state.pickle(b)
          case b: CustomField.Tag        .Id => state.enc.writeByte(KeyTag        ); state.pickle(b)
          case b: CustomField.Text       .Id => state.enc.writeByte(KeyText       ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): CustomFieldId =
        state.dec.readByte match {
          case KeyImplication => state.unpickle[CustomField.Implication.Id]
          case KeyTag         => state.unpickle[CustomField.Tag        .Id]
          case KeyText        => state.unpickle[CustomField.Text       .Id]
        }
    }

  implicit lazy val picklerCustomFieldImplicationId: Pickler[CustomField.Implication.Id] =
    pickleTaggedI(CustomField.Implication.Id).reuseByUnivEq

  implicit lazy val picklerCustomFieldTagId: Pickler[CustomField.Tag.Id] =
    pickleTaggedI(CustomField.Tag.Id).reuseByUnivEq

  implicit lazy val picklerCustomFieldTextId: Pickler[CustomField.Text.Id] =
    pickleTaggedI(CustomField.Text.Id).reuseByUnivEq

  implicit lazy val picklerCustomIssueTypeId: Pickler[CustomIssueTypeId] =
    pickleTaggedI(CustomIssueTypeId).reuseByUnivEq

  implicit lazy val picklerCustomReqTypeId: Pickler[CustomReqTypeId] =
    pickleTaggedI(CustomReqTypeId).reuseByUnivEq

  implicit lazy val picklerFilterDead: Pickler[FilterDead] =
    pickleBool(ShowDead)

  implicit lazy val picklerGenericReqId: Pickler[GenericReqId] =
    pickleTaggedI(GenericReqId).reuseByUnivEq

  implicit lazy val picklerHashRefKey: Pickler[HashRefKey] =
    pickleTaggedS(HashRefKey)

  implicit lazy val picklerIssueCategory: Pickler[IssueCategory] =
    new Pickler[IssueCategory] {
      import IssueCategory._
      private[this] final val KeyBadData     = 'b'
      private[this] final val KeyFutility    = 'f'
      private[this] final val KeyMissingData = 'm'
      private[this] final val KeyUserDefined = 'u'
      override def pickle(a: IssueCategory)(implicit state: PickleState): Unit =
        a match {
          case BadData     => state.enc.writeByte(KeyBadData    )
          case Futility    => state.enc.writeByte(KeyFutility   )
          case MissingData => state.enc.writeByte(KeyMissingData)
          case UserDefined => state.enc.writeByte(KeyUserDefined)
        }
      override def unpickle(implicit state: UnpickleState): IssueCategory =
        state.dec.readByte match {
          case KeyBadData     => BadData
          case KeyFutility    => Futility
          case KeyMissingData => MissingData
          case KeyUserDefined => UserDefined
        }
    }

  implicit lazy val picklerLive: Pickler[Live] =
    pickleBool(Live)

  implicit lazy val picklerMandatory: Pickler[Mandatory] =
    pickleBool(Mandatory)

  implicit lazy val picklerManualIssueId: Pickler[ManualIssueId] =
    pickleTaggedI(ManualIssueId)

  implicit lazy val picklerMultimapReqCodeValueSetApReqCodeId: Pickler[Multimap[ReqCode.Value, Set, ApReqCodeId]] =
    pickleMultimap[ReqCode.Value, Set, ApReqCodeId]

  implicit lazy val picklerExclusivity: Pickler[Exclusivity] =
    pickleBool(Exclusive)

  implicit lazy val picklerOn: Pickler[On] =
    pickleBool(On)

  implicit lazy val picklerReqCodeGroupId: Pickler[ReqCodeGroupId] =
    pickleTaggedI(ReqCodeGroupId).reuseByUnivEq

  implicit lazy val picklerReqCodeId: Pickler[ReqCodeId] =
    new Pickler[ReqCodeId] {
      private[this] final val KeyApReqCodeId    = 'a'
      private[this] final val KeyReqCodeGroupId = 'g'
      override def pickle(a: ReqCodeId)(implicit state: PickleState): Unit =
        a match {
          case b: ApReqCodeId    => state.enc.writeByte(KeyApReqCodeId   ); state.pickle(b)
          case b: ReqCodeGroupId => state.enc.writeByte(KeyReqCodeGroupId); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): ReqCodeId =
        state.dec.readByte match {
          case KeyApReqCodeId    => state.unpickle[ApReqCodeId]
          case KeyReqCodeGroupId => state.unpickle[ReqCodeGroupId]
        }
    }

  implicit lazy val picklerReqCodeNode: Pickler[ReqCode.Node] =
    transformPickler(ReqCode.Node.apply)(_.value) // xmap[String] already reuses

  implicit lazy val picklerReqCodeValue: Pickler[ReqCode.Value] =
    pickleNEV

  implicit lazy val picklerReqId: Pickler[ReqId] =
    new Pickler[ReqId] {
      private[this] final val KeyGenericReqId = 'g'
      private[this] final val KeyUseCaseId    = 'u'
      override def pickle(a: ReqId)(implicit state: PickleState): Unit =
        a match {
          case b: GenericReqId => state.enc.writeByte(KeyGenericReqId); state.pickle(b)
          case b: UseCaseId    => state.enc.writeByte(KeyUseCaseId   ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): ReqId =
        state.dec.readByte match {
          case KeyGenericReqId => state.unpickle[GenericReqId]
          case KeyUseCaseId    => state.unpickle[UseCaseId]
        }
    }

  implicit lazy val picklerReqTypeId: Pickler[ReqTypeId] =
    new Pickler[ReqTypeId] {
      private[this] final val KeyCustomReqTypeId = 'c'
      private[this] final val KeyUseCase         = 'u'
      override def pickle(a: ReqTypeId)(implicit state: PickleState): Unit =
        a match {
          case b: CustomReqTypeId    => state.enc.writeByte(KeyCustomReqTypeId); state.pickle(b)
          case StaticReqType.UseCase => state.enc.writeByte(KeyUseCase        )
        }
      override def unpickle(implicit state: UnpickleState): ReqTypeId =
        state.dec.readByte match {
          case KeyCustomReqTypeId => state.unpickle[CustomReqTypeId]
          case KeyUseCase         => StaticReqType.UseCase
        }
    }

  implicit lazy val picklerReqTypeMnemonic: Pickler[ReqType.Mnemonic] =
    pickleTaggedS(ReqType.Mnemonic)

  implicit lazy val picklerStaticFieldUseCaseStepTree: Pickler[StaticField.UseCaseStepTree] =
    new Pickler[StaticField.UseCaseStepTree] {
      private[this] final val KeyNormalAltStepTree = 'n'
      private[this] final val KeyExceptionStepTree = 'e'
      override def pickle(a: StaticField.UseCaseStepTree)(implicit state: PickleState): Unit =
        a match {
          case StaticField.NormalAltStepTree => state.enc.writeByte(KeyNormalAltStepTree)
          case StaticField.ExceptionStepTree => state.enc.writeByte(KeyExceptionStepTree)
        }
      override def unpickle(implicit state: UnpickleState): StaticField.UseCaseStepTree =
        state.dec.readByte match {
          case KeyNormalAltStepTree => StaticField.NormalAltStepTree
          case KeyExceptionStepTree => StaticField.ExceptionStepTree
        }
    }

  implicit lazy val picklerTagGroupId: Pickler[TagGroupId] =
    pickleTaggedI(TagGroupId).reuseByUnivEq

  implicit lazy val picklerTagId: Pickler[TagId] =
    new Pickler[TagId] {
      private[this] final val KeyApplicableTagId = 'a'
      private[this] final val KeyTagGroupId      = 'g'
      override def pickle(a: TagId)(implicit state: PickleState): Unit =
        a match {
          case b: ApplicableTagId => state.enc.writeByte(KeyApplicableTagId); state.pickle(b)
          case b: TagGroupId      => state.enc.writeByte(KeyTagGroupId     ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): TagId =
        state.dec.readByte match {
          case KeyApplicableTagId => state.unpickle[ApplicableTagId]
          case KeyTagGroupId      => state.unpickle[TagGroupId]
        }
    }

  implicit lazy val picklerUseCaseId: Pickler[UseCaseId] =
    pickleTaggedI(UseCaseId).reuseByUnivEq

  implicit lazy val picklerUseCaseStepId: Pickler[UseCaseStepId] =
    pickleTaggedI(UseCaseStepId).reuseByUnivEq

}
