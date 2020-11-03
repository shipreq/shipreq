package shipreq.webapp.base.protocol.binary.v1

import boopickle.DefaultBasic._
import java.time.Instant
import nyaya.util.Multimap
import shipreq.base.util.{Direction, Exclusivity, SetDiff}
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.ProjectText

/** This is all remaining codecs not covered by [[BaseMemberData1]].
  *
  * They typically all add up to a representation of [[Project]] which changes relatively frequently.
  */
object BaseMemberData2 {
  import BaseData._
  import BaseMemberData1._

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

  implicit lazy val picklerDeletionReasonIdO =
    optionPickler(pickleTaggedI(DeletionReasonId)).reuseByUnivEq

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

  implicit lazy val picklerImplicationGraph: Pickler[Implications.Graph] =
    pickleDigraphBiDir

  implicit lazy val picklerImplications: Pickler[Implications] =
    transformPickler(Implications.apply)(_.graph)

  implicit lazy val picklerMultimapReqIdSetApReqCodeId: Pickler[Multimap[ReqId, Set, ApReqCodeId]] =
    pickleMultimap[ReqId, Set, ApReqCodeId]

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

  implicit lazy val picklerReqDataTags: Pickler[ReqData.Tags] =
    pickleMultimap

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

  implicit lazy val picklerUseCasesStepFlow: Pickler[UseCases.StepFlow] =
    pickleDigraphBiDir

}
