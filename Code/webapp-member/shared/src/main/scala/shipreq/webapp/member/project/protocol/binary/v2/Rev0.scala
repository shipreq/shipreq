package shipreq.webapp.member.project.protocol.binary.v2

import java.time.Instant
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._

/** v2.0: For ShipReq Phase 3. */
object Rev0 {
  import boopickle.DefaultBasic._
  import shipreq.webapp.base.protocol.binary.v1.BaseData._
  import shipreq.webapp.member.project.protocol.binary.v1.BaseMemberData2._
  import shipreq.webapp.member.project.protocol.binary.v1.Rev1._
  import shipreq.webapp.member.project.protocol.binary.v1.Rev6._
  import shipreq.webapp.member.project.protocol.binary.v1.Rev7._
  import shipreq.webapp.member.project.protocol.binary.v1.Rev7.SavedViewPicklers._
  import shipreq.webapp.member.project.protocol.binary.v1.Events._
  import shipreq.webapp.member.project.protocol.binary.v1.PostEvents._

  implicit lazy val picklerProjectCreator: Pickler[ProjectCreator] =
    implicitly[Pickler[UserId.Public]].xmap(ProjectCreator.apply)(_.userId)

  implicit lazy val picklerProjectPerm: Pickler[ProjectPerm] =
    new Pickler[ProjectPerm] {
      // Note: 0 is reserved for Option[ProjectPerm]
      private[this] final val KeyAdmin        = 1
      private[this] final val KeyCollaborator = 2
      override def pickle(a: ProjectPerm)(implicit state: PickleState): Unit =
        a match {
          case ProjectPerm.Admin        => state.enc.writeByte(KeyAdmin       )
          case ProjectPerm.Collaborator => state.enc.writeByte(KeyCollaborator)
        }
      override def unpickle(implicit state: UnpickleState): ProjectPerm =
        state.dec.readByte match {
          case KeyAdmin        => ProjectPerm.Admin
          case KeyCollaborator => ProjectPerm.Collaborator
        }
    }

  implicit lazy val picklerOptionProjectPerm: Pickler[Option[ProjectPerm]] =
    new Pickler[Option[ProjectPerm]] {
      private[this] final val KeyNone = 0
      override def pickle(a: Option[ProjectPerm])(implicit state: PickleState): Unit =
        a match {
          case Some(p) => picklerProjectPerm.pickle(p)
          case None    => state.enc.writeByte(KeyNone)
        }
      override def unpickle(implicit state: UnpickleState): Option[ProjectPerm] =
        if (state.dec.peek(_.readByte) == KeyNone) {
          state.dec.readByte
          None
        } else
          Some(picklerProjectPerm.unpickle)
    }

  private[binary] implicit lazy val picklerEventAccessUpdate: Pickler[Event.AccessUpdate] =
    pickleMap[UserId.Public, Option[ProjectPerm]].xmap(Event.AccessUpdate.apply)(_.updates)

  implicit lazy val picklerEvent: Pickler[Event] =
    new Pickler[Event] {
      import Event._
      private[this] final val KeyApplicableTagCreateV1   =  0
      private[this] final val KeyApplicableTagUpdateV1   =  1
      private[this] final val KeyCodeGroupCreate         =  2
      private[this] final val KeyCodeGroupUpdate         =  3
      private[this] final val KeyCodeGroupsDelete        =  4
      private[this] final val KeyContentRestore          =  5
      private[this] final val KeyCustomIssueTypeCreate   =  6
      private[this] final val KeyCustomIssueTypeDelete   =  7
      private[this] final val KeyCustomIssueTypeRestore  =  8
      private[this] final val KeyCustomIssueTypeUpdate   =  9
      private[this] final val KeyCustomReqTypeCreateV1   = 10
      private[this] final val KeyCustomReqTypeDelete     = 11
      private[this] final val KeyCustomReqTypeRestore    = 12
      private[this] final val KeyCustomReqTypeUpdateV1   = 13
      private[this] final val KeyFieldCustomDelete       = 14
      private[this] final val KeyFieldCustomImpCreateV1  = 15
      private[this] final val KeyFieldCustomImpUpdateV1  = 16
      private[this] final val KeyFieldCustomRestore      = 17
      private[this] final val KeyFieldCustomTagCreateV1  = 18
      private[this] final val KeyFieldCustomTagUpdateV1  = 19
      private[this] final val KeyFieldCustomTextCreateV1 = 20
      private[this] final val KeyFieldCustomTextUpdateV1 = 21
      private[this] final val KeyFieldReposition         = 22
      private[this] final val KeyFieldStaticAdd          = 23
      private[this] final val KeyFieldStaticRemove       = 24
      private[this] final val KeyGenericReqCreate        = 25
      private[this] final val KeyGenericReqTitleSet      = 26
      private[this] final val KeyGenericReqTypeSet       = 27
      private[this] final val KeyManualIssueCreate       = 28
      private[this] final val KeyManualIssueDelete       = 29
      private[this] final val KeyManualIssueUpdate       = 30
      private[this] final val KeyProjectNameSet          = 31
      private[this] final val KeyProjectTemplateApply    = 32
      private[this] final val KeyReqCodesPatch           = 33
      private[this] final val KeyReqFieldCustomTextSet   = 34
      private[this] final val KeyReqImplicationsPatch    = 35
      private[this] final val KeyReqTagsPatch            = 36
      private[this] final val KeyReqsDelete              = 37
      private[this] final val KeySavedViewCreateV1       = 38
      private[this] final val KeySavedViewDefaultSet     = 39
      private[this] final val KeySavedViewDelete         = 40
      private[this] final val KeySavedViewUpdateV1       = 41
      private[this] final val KeyTagDelete               = 42
      private[this] final val KeyTagGroupCreate          = 43
      private[this] final val KeyTagGroupUpdate          = 44
      private[this] final val KeyTagRestore              = 45
      private[this] final val KeyUseCaseCreate           = 46
      private[this] final val KeyUseCaseStepCreate       = 47
      private[this] final val KeyUseCaseStepDelete       = 48
      private[this] final val KeyUseCaseStepRestore      = 49
      private[this] final val KeyUseCaseStepShiftLeft    = 50
      private[this] final val KeyUseCaseStepShiftRight   = 51
      private[this] final val KeyUseCaseStepUpdate       = 52
      private[this] final val KeyUseCaseTitleSet         = 53
      private[this] final val KeyApplicableTagCreateV2   = 54
      private[this] final val KeyApplicableTagUpdateV2   = 55
      private[this] final val KeyCustomReqTypeDeleteHard = 56
      private[this] final val KeyCustomReqTypeDeleteSoft = 57
      private[this] final val KeyFieldCustomImpCreateV2  = 58
      private[this] final val KeyFieldCustomImpUpdateV2  = 59
      private[this] final val KeyFieldCustomTagCreateV2  = 60
      private[this] final val KeyFieldCustomTagUpdateV2  = 61
      private[this] final val KeyFieldCustomTextCreateV2 = 62
      private[this] final val KeyFieldCustomTextUpdateV2 = 63
      private[this] final val KeyCustomReqTypeCreateV2   = 64
      private[this] final val KeyCustomReqTypeUpdateV2   = 65
      private[this] final val KeySavedViewCreateV2       = 66
      private[this] final val KeySavedViewUpdateV2       = 67
      private[this] final val KeyAccessUpdate            = 68

      override def pickle(a: Event)(implicit state: PickleState): Unit =
        a match {
          case b: AccessUpdate            => state.enc.writeByte(KeyAccessUpdate           ); state.pickle(b)
          case b: ApplicableTagCreate     => state.enc.writeByte(KeyApplicableTagCreateV2  ); state.pickle(b)
          case b: ApplicableTagCreateV1   => state.enc.writeByte(KeyApplicableTagCreateV1  ); state.pickle(b)
          case b: ApplicableTagUpdate     => state.enc.writeByte(KeyApplicableTagUpdateV2  ); state.pickle(b)
          case b: ApplicableTagUpdateV1   => state.enc.writeByte(KeyApplicableTagUpdateV1  ); state.pickle(b)
          case b: CodeGroupCreate         => state.enc.writeByte(KeyCodeGroupCreate        ); state.pickle(b)
          case b: CodeGroupsDelete        => state.enc.writeByte(KeyCodeGroupsDelete       ); state.pickle(b)
          case b: CodeGroupUpdate         => state.enc.writeByte(KeyCodeGroupUpdate        ); state.pickle(b)
          case b: ContentRestore          => state.enc.writeByte(KeyContentRestore         ); state.pickle(b)
          case b: CustomIssueTypeCreate   => state.enc.writeByte(KeyCustomIssueTypeCreate  ); state.pickle(b)
          case b: CustomIssueTypeDelete   => state.enc.writeByte(KeyCustomIssueTypeDelete  ); state.pickle(b)
          case b: CustomIssueTypeRestore  => state.enc.writeByte(KeyCustomIssueTypeRestore ); state.pickle(b)
          case b: CustomIssueTypeUpdate   => state.enc.writeByte(KeyCustomIssueTypeUpdate  ); state.pickle(b)
          case b: CustomReqTypeCreate     => state.enc.writeByte(KeyCustomReqTypeCreateV2  ); state.pickle(b)
          case b: CustomReqTypeCreateV1   => state.enc.writeByte(KeyCustomReqTypeCreateV1  ); state.pickle(b)
          case b: CustomReqTypeDelete     => state.enc.writeByte(KeyCustomReqTypeDelete    ); state.pickle(b)
          case b: CustomReqTypeDeleteHard => state.enc.writeByte(KeyCustomReqTypeDeleteHard); state.pickle(b)
          case b: CustomReqTypeDeleteSoft => state.enc.writeByte(KeyCustomReqTypeDeleteSoft); state.pickle(b)
          case b: CustomReqTypeRestore    => state.enc.writeByte(KeyCustomReqTypeRestore   ); state.pickle(b)
          case b: CustomReqTypeUpdate     => state.enc.writeByte(KeyCustomReqTypeUpdateV2  ); state.pickle(b)
          case b: CustomReqTypeUpdateV1   => state.enc.writeByte(KeyCustomReqTypeUpdateV1  ); state.pickle(b)
          case b: FieldCustomDelete       => state.enc.writeByte(KeyFieldCustomDelete      ); state.pickle(b)
          case b: FieldCustomImpCreate    => state.enc.writeByte(KeyFieldCustomImpCreateV2 ); state.pickle(b)
          case b: FieldCustomImpCreateV1  => state.enc.writeByte(KeyFieldCustomImpCreateV1 ); state.pickle(b)
          case b: FieldCustomImpUpdate    => state.enc.writeByte(KeyFieldCustomImpUpdateV2 ); state.pickle(b)
          case b: FieldCustomImpUpdateV1  => state.enc.writeByte(KeyFieldCustomImpUpdateV1 ); state.pickle(b)
          case b: FieldCustomRestore      => state.enc.writeByte(KeyFieldCustomRestore     ); state.pickle(b)
          case b: FieldCustomTagCreate    => state.enc.writeByte(KeyFieldCustomTagCreateV2 ); state.pickle(b)
          case b: FieldCustomTagCreateV1  => state.enc.writeByte(KeyFieldCustomTagCreateV1 ); state.pickle(b)
          case b: FieldCustomTagUpdate    => state.enc.writeByte(KeyFieldCustomTagUpdateV2 ); state.pickle(b)
          case b: FieldCustomTagUpdateV1  => state.enc.writeByte(KeyFieldCustomTagUpdateV1 ); state.pickle(b)
          case b: FieldCustomTextCreate   => state.enc.writeByte(KeyFieldCustomTextCreateV2); state.pickle(b)
          case b: FieldCustomTextCreateV1 => state.enc.writeByte(KeyFieldCustomTextCreateV1); state.pickle(b)
          case b: FieldCustomTextUpdate   => state.enc.writeByte(KeyFieldCustomTextUpdateV2); state.pickle(b)
          case b: FieldCustomTextUpdateV1 => state.enc.writeByte(KeyFieldCustomTextUpdateV1); state.pickle(b)
          case b: FieldReposition         => state.enc.writeByte(KeyFieldReposition        ); state.pickle(b)
          case b: FieldStaticAdd          => state.enc.writeByte(KeyFieldStaticAdd         ); state.pickle(b)
          case b: FieldStaticRemove       => state.enc.writeByte(KeyFieldStaticRemove      ); state.pickle(b)
          case b: GenericReqCreate        => state.enc.writeByte(KeyGenericReqCreate       ); state.pickle(b)
          case b: GenericReqTitleSet      => state.enc.writeByte(KeyGenericReqTitleSet     ); state.pickle(b)
          case b: GenericReqTypeSet       => state.enc.writeByte(KeyGenericReqTypeSet      ); state.pickle(b)
          case b: ManualIssueCreate       => state.enc.writeByte(KeyManualIssueCreate      ); state.pickle(b)
          case b: ManualIssueDelete       => state.enc.writeByte(KeyManualIssueDelete      ); state.pickle(b)
          case b: ManualIssueUpdate       => state.enc.writeByte(KeyManualIssueUpdate      ); state.pickle(b)
          case b: ProjectNameSet          => state.enc.writeByte(KeyProjectNameSet         ); state.pickle(b)
          case b: ProjectTemplateApply    => state.enc.writeByte(KeyProjectTemplateApply   ); state.pickle(b)
          case b: ReqCodesPatch           => state.enc.writeByte(KeyReqCodesPatch          ); state.pickle(b)
          case b: ReqFieldCustomTextSet   => state.enc.writeByte(KeyReqFieldCustomTextSet  ); state.pickle(b)
          case b: ReqImplicationsPatch    => state.enc.writeByte(KeyReqImplicationsPatch   ); state.pickle(b)
          case b: ReqsDelete              => state.enc.writeByte(KeyReqsDelete             ); state.pickle(b)
          case b: ReqTagsPatch            => state.enc.writeByte(KeyReqTagsPatch           ); state.pickle(b)
          case b: SavedViewCreate         => state.enc.writeByte(KeySavedViewCreateV2      ); state.pickle(b)
          case b: SavedViewCreateV1       => state.enc.writeByte(KeySavedViewCreateV1      ); state.pickle(b)
          case b: SavedViewDefaultSet     => state.enc.writeByte(KeySavedViewDefaultSet    ); state.pickle(b)
          case b: SavedViewDelete         => state.enc.writeByte(KeySavedViewDelete        ); state.pickle(b)
          case b: SavedViewUpdate         => state.enc.writeByte(KeySavedViewUpdateV2      ); state.pickle(b)
          case b: SavedViewUpdateV1       => state.enc.writeByte(KeySavedViewUpdateV1      ); state.pickle(b)
          case b: TagDelete               => state.enc.writeByte(KeyTagDelete              ); state.pickle(b)
          case b: TagGroupCreate          => state.enc.writeByte(KeyTagGroupCreate         ); state.pickle(b)
          case b: TagGroupUpdate          => state.enc.writeByte(KeyTagGroupUpdate         ); state.pickle(b)
          case b: TagRestore              => state.enc.writeByte(KeyTagRestore             ); state.pickle(b)
          case b: UseCaseCreate           => state.enc.writeByte(KeyUseCaseCreate          ); state.pickle(b)
          case b: UseCaseStepCreate       => state.enc.writeByte(KeyUseCaseStepCreate      ); state.pickle(b)
          case b: UseCaseStepDelete       => state.enc.writeByte(KeyUseCaseStepDelete      ); state.pickle(b)
          case b: UseCaseStepRestore      => state.enc.writeByte(KeyUseCaseStepRestore     ); state.pickle(b)
          case b: UseCaseStepShiftLeft    => state.enc.writeByte(KeyUseCaseStepShiftLeft   ); state.pickle(b)
          case b: UseCaseStepShiftRight   => state.enc.writeByte(KeyUseCaseStepShiftRight  ); state.pickle(b)
          case b: UseCaseStepUpdate       => state.enc.writeByte(KeyUseCaseStepUpdate      ); state.pickle(b)
          case b: UseCaseTitleSet         => state.enc.writeByte(KeyUseCaseTitleSet        ); state.pickle(b)
        }

      override def unpickle(implicit state: UnpickleState): Event =
        state.dec.readByte match {
          case KeyAccessUpdate            => state.unpickle[AccessUpdate]
          case KeyApplicableTagCreateV1   => state.unpickle[ApplicableTagCreateV1]
          case KeyApplicableTagCreateV2   => state.unpickle[ApplicableTagCreate]
          case KeyApplicableTagUpdateV1   => state.unpickle[ApplicableTagUpdateV1]
          case KeyApplicableTagUpdateV2   => state.unpickle[ApplicableTagUpdate]
          case KeyCodeGroupCreate         => state.unpickle[CodeGroupCreate]
          case KeyCodeGroupsDelete        => state.unpickle[CodeGroupsDelete]
          case KeyCodeGroupUpdate         => state.unpickle[CodeGroupUpdate]
          case KeyContentRestore          => state.unpickle[ContentRestore]
          case KeyCustomIssueTypeCreate   => state.unpickle[CustomIssueTypeCreate]
          case KeyCustomIssueTypeDelete   => state.unpickle[CustomIssueTypeDelete]
          case KeyCustomIssueTypeRestore  => state.unpickle[CustomIssueTypeRestore]
          case KeyCustomIssueTypeUpdate   => state.unpickle[CustomIssueTypeUpdate]
          case KeyCustomReqTypeCreateV1   => state.unpickle[CustomReqTypeCreateV1]
          case KeyCustomReqTypeCreateV2   => state.unpickle[CustomReqTypeCreate]
          case KeyCustomReqTypeDelete     => state.unpickle[CustomReqTypeDelete]
          case KeyCustomReqTypeDeleteHard => state.unpickle[CustomReqTypeDeleteHard]
          case KeyCustomReqTypeDeleteSoft => state.unpickle[CustomReqTypeDeleteSoft]
          case KeyCustomReqTypeRestore    => state.unpickle[CustomReqTypeRestore]
          case KeyCustomReqTypeUpdateV1   => state.unpickle[CustomReqTypeUpdateV1]
          case KeyCustomReqTypeUpdateV2   => state.unpickle[CustomReqTypeUpdate]
          case KeyFieldCustomDelete       => state.unpickle[FieldCustomDelete]
          case KeyFieldCustomImpCreateV1  => state.unpickle[FieldCustomImpCreateV1]
          case KeyFieldCustomImpCreateV2  => state.unpickle[FieldCustomImpCreate]
          case KeyFieldCustomImpUpdateV1  => state.unpickle[FieldCustomImpUpdateV1]
          case KeyFieldCustomImpUpdateV2  => state.unpickle[FieldCustomImpUpdate]
          case KeyFieldCustomRestore      => state.unpickle[FieldCustomRestore]
          case KeyFieldCustomTagCreateV1  => state.unpickle[FieldCustomTagCreateV1]
          case KeyFieldCustomTagCreateV2  => state.unpickle[FieldCustomTagCreate]
          case KeyFieldCustomTagUpdateV1  => state.unpickle[FieldCustomTagUpdateV1]
          case KeyFieldCustomTagUpdateV2  => state.unpickle[FieldCustomTagUpdate]
          case KeyFieldCustomTextCreateV1 => state.unpickle[FieldCustomTextCreateV1]
          case KeyFieldCustomTextCreateV2 => state.unpickle[FieldCustomTextCreate]
          case KeyFieldCustomTextUpdateV1 => state.unpickle[FieldCustomTextUpdateV1]
          case KeyFieldCustomTextUpdateV2 => state.unpickle[FieldCustomTextUpdate]
          case KeyFieldReposition         => state.unpickle[FieldReposition]
          case KeyFieldStaticAdd          => state.unpickle[FieldStaticAdd]
          case KeyFieldStaticRemove       => state.unpickle[FieldStaticRemove]
          case KeyGenericReqCreate        => state.unpickle[GenericReqCreate]
          case KeyGenericReqTitleSet      => state.unpickle[GenericReqTitleSet]
          case KeyGenericReqTypeSet       => state.unpickle[GenericReqTypeSet]
          case KeyManualIssueCreate       => state.unpickle[ManualIssueCreate]
          case KeyManualIssueDelete       => state.unpickle[ManualIssueDelete]
          case KeyManualIssueUpdate       => state.unpickle[ManualIssueUpdate]
          case KeyProjectNameSet          => state.unpickle[ProjectNameSet]
          case KeyProjectTemplateApply    => state.unpickle[ProjectTemplateApply]
          case KeyReqCodesPatch           => state.unpickle[ReqCodesPatch]
          case KeyReqFieldCustomTextSet   => state.unpickle[ReqFieldCustomTextSet]
          case KeyReqImplicationsPatch    => state.unpickle[ReqImplicationsPatch]
          case KeyReqsDelete              => state.unpickle[ReqsDelete]
          case KeyReqTagsPatch            => state.unpickle[ReqTagsPatch]
          case KeySavedViewCreateV1       => state.unpickle[SavedViewCreateV1]
          case KeySavedViewCreateV2       => state.unpickle[SavedViewCreate]
          case KeySavedViewDefaultSet     => state.unpickle[SavedViewDefaultSet]
          case KeySavedViewDelete         => state.unpickle[SavedViewDelete]
          case KeySavedViewUpdateV1       => state.unpickle[SavedViewUpdateV1]
          case KeySavedViewUpdateV2       => state.unpickle[SavedViewUpdate]
          case KeyTagDelete               => state.unpickle[TagDelete]
          case KeyTagGroupCreate          => state.unpickle[TagGroupCreate]
          case KeyTagGroupUpdate          => state.unpickle[TagGroupUpdate]
          case KeyTagRestore              => state.unpickle[TagRestore]
          case KeyUseCaseCreate           => state.unpickle[UseCaseCreate]
          case KeyUseCaseStepCreate       => state.unpickle[UseCaseStepCreate]
          case KeyUseCaseStepDelete       => state.unpickle[UseCaseStepDelete]
          case KeyUseCaseStepRestore      => state.unpickle[UseCaseStepRestore]
          case KeyUseCaseStepShiftLeft    => state.unpickle[UseCaseStepShiftLeft]
          case KeyUseCaseStepShiftRight   => state.unpickle[UseCaseStepShiftRight]
          case KeyUseCaseStepUpdate       => state.unpickle[UseCaseStepUpdate]
          case KeyUseCaseTitleSet         => state.unpickle[UseCaseTitleSet]
        }
    }

  implicit lazy val picklerActiveEvent: Pickler[ActiveEvent] =
    picklerEvent.narrow

  implicit lazy val picklerVerifiedEvent: Pickler[VerifiedEvent] =
    new Pickler[VerifiedEvent] {
      override def pickle(a: VerifiedEvent)(implicit state: PickleState): Unit = {
        state.pickle(a.ord)
        state.pickle(a.event)
        state.pickle(a.createdAt)
      }
      override def unpickle(implicit state: UnpickleState): VerifiedEvent = {
        val ord       = state.unpickle[EventOrd]
        val event     = state.unpickle[Event]
        val createdAt = state.unpickle[Instant]
        VerifiedEvent(ord, event, createdAt)
      }
    }

  implicit lazy val picklerVerifiedEventSeq: Pickler[VerifiedEvent.Seq] =
    iterablePickler

  implicit lazy val picklerVerifiedEventNonEmptySeq: Pickler[VerifiedEvent.NonEmptySeq] =
    new Pickler[VerifiedEvent.NonEmptySeq] {
      override def pickle(a: VerifiedEvent.NonEmptySeq)(implicit state: PickleState): Unit = {
        state.pickle(a.head)
        state.pickle(a.tail)
      }
      override def unpickle(implicit state: UnpickleState): VerifiedEvent.NonEmptySeq = {
        val head = state.unpickle[VerifiedEvent]
        val tail = state.unpickle[VerifiedEvent.Seq]
        VerifiedEvent.NonEmptySeq(head, tail)
      }
    }

  implicit lazy val picklerErrorMsgOrVerifiedEventSeq: Pickler[ErrorMsg \/ VerifiedEvent.Seq] =
    pickleDisj

  implicit lazy val picklerProjectEvents: Pickler[ProjectEvents] =
    transformPickler(ProjectEvents.apply)(_.events)

  implicit lazy val picklerClientSideProjectEncryptionKey: Pickler[ClientSideProjectEncryptionKey] =
    transformPickler(ClientSideProjectEncryptionKey.apply)(_.value)

  implicit lazy val picklerProjectAccess: Pickler[ProjectAccess] =
    pickleMap[UserId.Public, ProjectPerm].xmap(ProjectAccess.apply)(_.asMap)

  implicit lazy val picklerProject: Pickler[Project] =
    new Pickler[Project] {
      override def pickle(p: Project)(implicit state: PickleState): Unit = {
        state.pickle(p.name)
        state.pickle(p.config)
        state.pickle(p.content)
        state.pickle(p.manualIssues)
        state.pickle(p.savedViews)
        state.pickle(p.access)
        state.pickle(p.history)
        state.pickle(p.idCeilings)
      }
      override def unpickle(implicit state: UnpickleState): Project = {
        val name          = state.unpickle[Project.Name]
        val config        = state.unpickle[ProjectConfig]
        val content       = state.unpickle[ProjectContent]
        val manualIssues  = state.unpickle[ManualIssues]
        val savedViews    = state.unpickle[savedview.SavedViews.Optional]
        val access        = state.unpickle[ProjectAccess]
        val history       = state.unpickle[ProjectEvents]
        val idCeilings    = state.unpickle[IdCeilings]
        Project(name, config, content, manualIssues, savedViews, access, history, idCeilings)
      }
    }

  implicit lazy val picklerProjectOrEvents: Pickler[Project \/ VerifiedEvent.Seq] =
    pickleDisj

  implicit lazy val picklerProjectMetaData: Pickler[ProjectMetaData] =
    new Pickler[ProjectMetaData] {
      override def pickle(a: ProjectMetaData)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.perm)
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
        val perm          = state.unpickle[Option[ProjectPerm]]
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
          perm          = perm         ,
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

  implicit lazy val picklerRolodex: Pickler[Rolodex] =
    pickleMap[UserId.Public, Username].xmap(Rolodex.apply)(_.asMap)

}
