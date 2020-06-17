package shipreq.webapp.base.protocol.binary.v1

import boopickle.DefaultBasic._
import java.time.Instant
import nyaya.util.Multimap
import shipreq.base.util.{Direction, Exclusivity, SetDiff}
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{ProjectText, Text}

/** This is all remaining codecs not covered by [[BaseMemberData1]].
  *
  * They typically all add up to a representation of [[Project]] which changes relatively frequently.
  */
object BaseMemberData2 {
  import BaseData._
  import BaseMemberData1._
  import BaseMemberData1.AtomPicklers.instances._

  implicit lazy val picklerCodeGroup: Pickler[CodeGroup] =
    new Pickler[CodeGroup] {
      private[this] final val KeyDeadCodeGroup = 'd'
      private[this] final val KeyLiveCodeGroup = 'l'
      override def pickle(a: CodeGroup)(implicit state: PickleState): Unit =
        a match {
          case b: DeadCodeGroup => state.enc.writeByte(KeyDeadCodeGroup); state.pickle(b)
          case b: LiveCodeGroup => state.enc.writeByte(KeyLiveCodeGroup); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): CodeGroup =
        state.dec.readByte match {
          case KeyDeadCodeGroup => state.unpickle[DeadCodeGroup]
          case KeyLiveCodeGroup => state.unpickle[LiveCodeGroup]
        }
    }

  implicit lazy val picklerCustomFieldType: Pickler[CustomFieldType] =
    new Pickler[CustomFieldType] {
      private[this] final val KeyImplication = 'i'
      private[this] final val KeyTag         = 't'
      private[this] final val KeyText        = 'x'
      override def pickle(a: CustomFieldType)(implicit state: PickleState): Unit =
        a match {
          case CustomFieldType.Implication => state.enc.writeByte(KeyImplication)
          case CustomFieldType.Tag         => state.enc.writeByte(KeyTag        )
          case CustomFieldType.Text        => state.enc.writeByte(KeyText       )
        }
      override def unpickle(implicit state: UnpickleState): CustomFieldType =
        state.dec.readByte match {
          case KeyImplication => CustomFieldType.Implication
          case KeyTag         => CustomFieldType.Tag
          case KeyText        => CustomFieldType.Text
        }
    }

  implicit lazy val picklerCustomIssueType: Pickler[CustomIssueType] =
    new Pickler[CustomIssueType] {
      override def pickle(a: CustomIssueType)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.key)
        state.pickle(a.desc)
        state.pickle(a.live)
      }
      override def unpickle(implicit state: UnpickleState): CustomIssueType = {
        val id   = state.unpickle[CustomIssueTypeId]
        val key  = state.unpickle[HashRefKey]
        val desc = state.unpickle[Option[String]]
        val live = state.unpickle[Live]
        CustomIssueType(id, key, desc, live)
      }
    }

  implicit lazy val picklerCustomIssueTypes: Pickler[CustomIssueTypeIMap] =
    pickleIMapD

  implicit lazy val picklerDeadCodeGroup: Pickler[DeadCodeGroup] =
    new Pickler[DeadCodeGroup] {
      override def pickle(a: DeadCodeGroup)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.title)
      }
      override def unpickle(implicit state: UnpickleState): DeadCodeGroup = {
        val id    = state.unpickle[ReqCodeGroupId]
        val title = state.unpickle[Text.CodeGroupTitle.OptionalText]
        DeadCodeGroup(id, title)
      }
    }
  implicit lazy val picklerDeletionReasonIdO =
    optionPickler(pickleTaggedI(DeletionReasonId)).reuseByUnivEq

  implicit lazy val picklerDeletionReasons: Pickler[DeletionReasons] =
    new Pickler[DeletionReasons] {
      private[this] implicit val picklerRA: Pickler[DeletionReasons.ReqApplication] = pickleMultimap
      override def pickle(a: DeletionReasons)(implicit state: PickleState): Unit = {
        state.pickle(a.reasons)
        state.pickle(a.reqApplication)
      }
      override def unpickle(implicit state: UnpickleState): DeletionReasons = {
        val reasons        = state.unpickle[Vector[Text.DeletionReason.NonEmptyText]]
        val reqApplication = state.unpickle[DeletionReasons.ReqApplication]
        DeletionReasons(reasons, reqApplication)
      }
    }

  implicit lazy val picklerGenericReq: Pickler[GenericReq] =
    new Pickler[GenericReq] {
      override def pickle(a: GenericReq)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.pubid)
        state.pickle(a.title)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): GenericReq = {
        val id             = state.unpickle[GenericReqId]
        val pubid          = state.unpickle[PubidC]
        val title          = state.unpickle[Text.GenericReqTitle.OptionalText]
        val liveExplicitly = state.unpickle[Live]
        GenericReq(id, pubid, title, liveExplicitly)
      }
    }

  implicit lazy val picklerGenericReqsById: Pickler[GenericReqIMap] =
    pickleIMapD

  implicit lazy val picklerGenericReqs: Pickler[GenericReqs] =
    picklerGenericReqsById.xmap(GenericReqs.apply)(_.imap)

  implicit lazy val picklerIdCeilings: Pickler[IdCeilings] =
    new Pickler[IdCeilings] {
      override def pickle(a: IdCeilings)(implicit state: PickleState): Unit = {
        state.pickle(a.customIssueType)
        state.pickle(a.customReqType)
        state.pickle(a.customField)
        state.pickle(a.tag)
        state.pickle(a.req)
        state.pickle(a.useCaseStep)
        state.pickle(a.reqCode)
        state.pickle(a.savedView)
      }
      override def unpickle(implicit state: UnpickleState): IdCeilings = {
        val customIssueType = state.unpickle[Int]
        val customReqType   = state.unpickle[Int]
        val customField     = state.unpickle[Int]
        val tag             = state.unpickle[Int]
        val req             = state.unpickle[Int]
        val useCaseStep     = state.unpickle[Int]
        val reqCode         = state.unpickle[Int]
        val savedView       = state.unpickle[Int]
        IdCeilings(customIssueType, customReqType, customField, tag, req, useCaseStep, reqCode, savedView)
      }
    }

  implicit lazy val picklerImplications: Pickler[Implications] =
    pickleDigraphBiDir

  implicit lazy val picklerLiveCodeGroup: Pickler[LiveCodeGroup] =
    new Pickler[LiveCodeGroup] {
      override def pickle(a: LiveCodeGroup)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.title)
      }
      override def unpickle(implicit state: UnpickleState): LiveCodeGroup = {
        val id    = state.unpickle[ReqCodeGroupId]
        val title = state.unpickle[Text.CodeGroupTitle.OptionalText]
        LiveCodeGroup(id, title)
      }
    }

  implicit lazy val picklerManualIssue: Pickler[ManualIssue] =
    new Pickler[ManualIssue] {
      override def pickle(a: ManualIssue)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.text)
      }
      override def unpickle(implicit state: UnpickleState): ManualIssue = {
        val id   = state.unpickle[ManualIssueId]
        val text = state.unpickle[Text.ManualIssue.NonEmptyText]
        ManualIssue(id, text)
      }
    }

  implicit lazy val picklerManualIssueIMap: Pickler[ManualIssue.IMap] =
    pickleIMap(ManualIssue.emptyIMap)

  implicit lazy val picklerManualIssues: Pickler[ManualIssues] =
    new Pickler[ManualIssues] {
      override def pickle(a: ManualIssues)(implicit state: PickleState): Unit = {
        state.pickle(a.imap)
        state.pickle(a.nextId)
      }
      override def unpickle(implicit state: UnpickleState): ManualIssues = {
        val imap   = state.unpickle[ManualIssue.IMap]
        val nextId = state.unpickle[ManualIssueId]
        ManualIssues(imap, nextId)
      }
    }

  implicit lazy val picklerMultimapReqIdSetApReqCodeId: Pickler[Multimap[ReqId, Set, ApReqCodeId]] =
    pickleMultimap[ReqId, Set, ApReqCodeId]

  implicit lazy val picklerProjectContent: Pickler[ProjectContent] =
    new Pickler[ProjectContent] {
      override def pickle(a: ProjectContent)(implicit state: PickleState): Unit = {
        state.pickle(a.reqs)
        state.pickle(a.reqCodes)
        state.pickle(a.reqText)
        state.pickle(a.reqTags)
        state.pickle(a.implications)
        state.pickle(a.deletionReasons)
      }
      override def unpickle(implicit state: UnpickleState): ProjectContent = {
        val reqs            = state.unpickle[Requirements]
        val reqCodes        = state.unpickle[ReqCodes]
        val reqText         = state.unpickle[ReqData.Text]
        val reqTags         = state.unpickle[ReqData.Tags]
        val implications    = state.unpickle[Implications.BiDir]
        val deletionReasons = state.unpickle[DeletionReasons]
        ProjectContent(reqs, reqCodes, reqText, reqTags, implications, deletionReasons)
      }
    }

  implicit lazy val picklerProjectMetaData: Pickler[ProjectMetaData] =
    new Pickler[ProjectMetaData] {
      override def pickle(a: ProjectMetaData)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.name)
        state.pickle(a.eventsInit)
        state.pickle(a.eventsTotal)
        state.pickle(a.reqsLive)
        state.pickle(a.reqsTotal)
        state.pickle(a.createdAt)
        state.pickle(a.accessedAt)
        state.pickle(a.lastUpdatedAt)
      }
      override def unpickle(implicit state: UnpickleState): ProjectMetaData = {
        val id            = state.unpickle[ProjectId.Public]
        val name          = state.unpickle[Project.Name]
        val eventsInit    = state.unpickle[Int]
        val eventsTotal   = state.unpickle[Int]
        val reqsLive      = state.unpickle[Int]
        val reqsTotal     = state.unpickle[Int]
        val createdAt     = state.unpickle[Instant]
        val accessedAt    = state.unpickle[Instant]
        val lastUpdatedAt = state.unpickle[Option[Instant]]
        ProjectMetaData(
          id            = id           ,
          name          = name         ,
          eventsInit    = eventsInit   ,
          eventsTotal   = eventsTotal  ,
          reqsLive      = reqsLive     ,
          reqsTotal     = reqsTotal    ,
          createdAt     = createdAt    ,
          accessedAt    = accessedAt   ,
          lastUpdatedAt = lastUpdatedAt)
      }
    }

  implicit lazy val picklerProjectTextContext: Pickler[ProjectText.Context] =
    new Pickler[ProjectText.Context] {
      import ProjectText.Context._
      private[this] implicit val picklerReq: Pickler[Req] = transformPickler(Req.apply)(_.id)
      private[this] final val KeyNone = 0
      private[this] final val KeyReq  = 'r'
      override def pickle(a: ProjectText.Context)(implicit state: PickleState): Unit =
        a match {
          case None    => state.enc.writeByte(KeyNone)
          case b: Req  => state.enc.writeByte(KeyReq ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): ProjectText.Context =
        state.dec.readByte match {
          case KeyNone => None
          case KeyReq  => state.unpickle[Req]
        }
    }

  implicit lazy val picklerPubid: Pickler[Pubid] =
    new Pickler[Pubid] {
      override def pickle(a: Pubid)(implicit state: PickleState): Unit = {
        state.pickle(a.reqTypeId)
        state.pickle(a.pos)
      }
      override def unpickle(implicit state: UnpickleState): Pubid = {
        val reqTypeId = state.unpickle[ReqTypeId]
        val pos       = state.unpickle[ReqTypePos]
        PubidT(reqTypeId, pos)
      }
    }

  implicit def picklerPubidT[T <: ReqTypeId]: Pickler[PubidT[T]] =
    picklerPubid.asInstanceOf[Pickler[PubidT[T]]]

  implicit lazy val picklerPubidRegister: Pickler[PubidRegister] =
    transformPickler(PubidRegister.apply)(_.value)(pickleMultimap)

  implicit lazy val picklerReq: Pickler[Req] =
    new Pickler[Req] {
      private[this] final val KeyGenericReq = 'g'
      private[this] final val KeyUseCase    = 'u'
      override def pickle(a: Req)(implicit state: PickleState): Unit =
        a match {
          case b: GenericReq => state.enc.writeByte(KeyGenericReq); state.pickle(b)
          case b: UseCase    => state.enc.writeByte(KeyUseCase   ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): Req =
        state.dec.readByte match {
          case KeyGenericReq => state.unpickle[GenericReq]
          case KeyUseCase    => state.unpickle[UseCase]
        }
    }

  implicit lazy val picklerReqCodeData: Pickler[ReqCode.Data] = {
    import ReqCode._

    implicit val picklerInactive: Pickler[Inactive] =
      new Pickler[Inactive] {
        override def pickle(a: Inactive)(implicit state: PickleState): Unit = {
          state.pickle(a.deadGroup)
          state.pickle(a.reqInactive)
        }
        override def unpickle(implicit state: UnpickleState): Inactive = {
          val deadGroup   = state.unpickle[DeadGroup]
          val reqInactive = state.unpickle[ReqInactive]
          Inactive(deadGroup, reqInactive)
        }
      }

    implicit val picklerActiveReq: Pickler[ActiveReq] =
      new Pickler[ActiveReq] {
        override def pickle(a: ActiveReq)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.reqId)
          state.pickle(a.deadGroup)
          state.pickle(a.reqInactive)
        }
        override def unpickle(implicit state: UnpickleState): ActiveReq = {
          val id          = state.unpickle[ApReqCodeId]
          val reqId       = state.unpickle[ReqId]
          val deadGroup   = state.unpickle[DeadGroup]
          val reqInactive = state.unpickle[ReqInactive]
          ActiveReq(id, reqId, deadGroup, reqInactive)
        }
      }

    implicit val picklerActiveGroup: Pickler[ActiveGroup] =
      new Pickler[ActiveGroup] {
        override def pickle(a: ActiveGroup)(implicit state: PickleState): Unit = {
          state.pickle(a.group)
          state.pickle(a.reqInactive)
        }
        override def unpickle(implicit state: UnpickleState): ActiveGroup = {
          val group       = state.unpickle[LiveCodeGroup]
          val reqInactive = state.unpickle[ReqInactive]
          ActiveGroup(group, reqInactive)
        }
      }

    new Pickler[Data] {
      private[this] final val KeyActiveGroup = 'g'
      private[this] final val KeyActiveReq   = 'r'
      private[this] final val KeyInactive    = 'i'
      override def pickle(a: Data)(implicit state: PickleState): Unit =
        a match {
          case b: ActiveGroup => state.enc.writeByte(KeyActiveGroup); state.pickle(b)
          case b: ActiveReq   => state.enc.writeByte(KeyActiveReq  ); state.pickle(b)
          case b: Inactive    => state.enc.writeByte(KeyInactive   ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): Data =
        state.dec.readByte match {
          case KeyActiveGroup => state.unpickle[ActiveGroup]
          case KeyActiveReq   => state.unpickle[ActiveReq]
          case KeyInactive    => state.unpickle[Inactive]
        }
    }
  }

  implicit lazy val picklerReqCodes: Pickler[ReqCodes] =
    transformPickler(ReqCodes.apply)(_.trie)

  implicit lazy val picklerReqCodeTrie: Pickler[ReqCode.Trie] =
    pickleTrie

  implicit lazy val picklerReqDataTags: Pickler[ReqData.Tags] =
    pickleMultimap

  implicit lazy val picklerReqDataText: Pickler[ReqData.Text] =
    pickleMap[CustomField.Text.Id, Map[ReqId, Text.CustomTextField.NonEmptyText]]
      .xmap(ReqData.Text.apply)(_.data)

  implicit lazy val picklerReqIdsByDirection: Pickler[Direction.Values[Set[ReqId]]] =
    pickleIsoBoolValues

  implicit lazy val picklerReqOrSubReqId: Pickler[ReqOrSubReqId] =
    new Pickler[ReqOrSubReqId] {
      private[this] final val KeyGenericReqId  = 'g'
      private[this] final val KeyUseCaseId     = 'u'
      private[this] final val KeyUseCaseStepId = 's'
      override def pickle(a: ReqOrSubReqId)(implicit state: PickleState): Unit =
        a match {
          case b: GenericReqId  => state.enc.writeByte(KeyGenericReqId ); state.pickle(b)
          case b: UseCaseId     => state.enc.writeByte(KeyUseCaseId    ); state.pickle(b)
          case b: UseCaseStepId => state.enc.writeByte(KeyUseCaseStepId); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): ReqOrSubReqId =
        state.dec.readByte match {
          case KeyGenericReqId  => state.unpickle[GenericReqId]
          case KeyUseCaseId     => state.unpickle[UseCaseId]
          case KeyUseCaseStepId => state.unpickle[UseCaseStepId]
        }
    }

  implicit lazy val picklerReqTypePos: Pickler[ReqTypePos] =
    pickleTaggedI(ReqTypePos)

  implicit lazy val picklerSetDiffReqCodeValue: Pickler[SetDiff[ReqCode.Value]] = pickleSetDiff

  implicit lazy val picklerSetDiffNEReqCodeValue: Pickler[SetDiff.NE[ReqCode.Value]] = pickleNonEmptyMono

  implicit lazy val picklerStaticReqType: Pickler[StaticReqType] =
    new Pickler[StaticReqType] {
      import StaticReqType._
      private[this] final val KeyUseCase = 'u'
      override def pickle(a: StaticReqType)(implicit state: PickleState): Unit =
        a match {
          case UseCase => state.enc.writeByte(KeyUseCase)
        }
      override def unpickle(implicit state: UnpickleState): StaticReqType =
        state.dec.readByte match {
          case KeyUseCase => UseCase
        }
    }

  implicit lazy val picklerSubReqId: Pickler[SubReqId] =
    new Pickler[SubReqId] {
      private[this] final val KeyUseCaseStepId = 's'
      override def pickle(a: SubReqId)(implicit state: PickleState): Unit =
        a match {
          case b: UseCaseStepId => state.enc.writeByte(KeyUseCaseStepId); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): SubReqId =
        state.dec.readByte match {
          case KeyUseCaseStepId => state.unpickle[UseCaseStepId]
        }
    }

  implicit lazy val picklerTagGroup: Pickler[TagGroup] =
    new Pickler[TagGroup] {
      override def pickle(a: TagGroup)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.name)
        state.pickle(a.desc)
        state.pickle(a.exclusivity)
        state.pickle(a.live)
      }
      override def unpickle(implicit state: UnpickleState): TagGroup = {
        val id          = state.unpickle[TagGroupId]
        val name        = state.unpickle[String]
        val desc        = state.unpickle[Option[String]]
        val exclusivity = state.unpickle[Exclusivity]
        val live        = state.unpickle[Live]
        TagGroup(id, name, desc, exclusivity, live)
      }
    }

  implicit lazy val picklerTagPovRelations: Pickler[TagInTree.Relations] =
    pickleMMTreeRelations

  implicit lazy val picklerUseCase: Pickler[UseCase] =
    new Pickler[UseCase] {
      override def pickle(a: UseCase)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.pos)
        state.pickle(a.title)
        state.pickle(a.stepsNA)
        state.pickle(a.stepsE)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): UseCase = {
        val id             = state.unpickle[UseCaseId]
        val pos            = state.unpickle[ReqTypePos]
        val title          = state.unpickle[Text.UseCaseTitle.OptionalText]
        val stepsNA        = state.unpickle[UseCaseSteps]
        val stepsE         = state.unpickle[UseCaseSteps]
        val liveExplicitly = state.unpickle[Live]
        UseCase(id, pos, title, stepsNA, stepsE, liveExplicitly)
      }
    }

  implicit lazy val picklerUseCases: Pickler[UseCases] =
    picklerUseCasesStateless imap UseCases.statelessIso

  implicit lazy val picklerUseCasesStateless: Pickler[UseCases.Stateless] =
    new Pickler[UseCases.Stateless] {
      override def pickle(a: UseCases.Stateless)(implicit state: PickleState): Unit = {
        state.pickle(a.imap)
        state.pickle(a.stepFlow)
      }
      override def unpickle(implicit state: UnpickleState): UseCases.Stateless = {
        val imap     = state.unpickle[UseCaseIMap]
        val stepFlow = state.unpickle[UseCases.StepFlow]
        UseCases.Stateless(imap, stepFlow)
      }
    }

  implicit lazy val picklerUseCasesById: Pickler[UseCaseIMap] =
    pickleIMapD

  implicit lazy val picklerUseCaseStep: Pickler[UseCaseStep] =
    new Pickler[UseCaseStep] {
      override def pickle(a: UseCaseStep)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.titleExplicitly)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): UseCaseStep = {
        val id              = state.unpickle[UseCaseStepId]
        val titleExplicitly = state.unpickle[Text.UseCaseStep.OptionalText]
        val liveExplicitly  = state.unpickle[Live]
        UseCaseStep(id, titleExplicitly, liveExplicitly)
      }
    }

  implicit lazy val picklerUseCasesStepFlow: Pickler[UseCases.StepFlow] =
    pickleDigraphBiDir

  implicit lazy val picklerUseCaseSteps: Pickler[UseCaseSteps] =
    transformPickler(UseCaseSteps.apply)(_.tree)(pickleVectorTree)

}
