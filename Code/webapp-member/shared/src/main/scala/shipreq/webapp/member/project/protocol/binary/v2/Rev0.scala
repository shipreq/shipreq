package shipreq.webapp.member.project.protocol.binary.v2

import java.time.Instant
import shipreq.webapp.base.data._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._

/** v2.0: For ShipReq Phase 3. */
object Rev0 {
  import boopickle.DefaultBasic._
  import shipreq.webapp.base.protocol.binary.v1.BaseData._
  import shipreq.webapp.member.project.protocol.binary.v1.BaseMemberData2._
  import shipreq.webapp.member.project.protocol.binary.v1.Rev6._
  import shipreq.webapp.member.project.protocol.binary.v1.Rev7._
  import shipreq.webapp.member.project.protocol.binary.v1.Rev7.SavedViewPicklers._

  implicit lazy val picklerProjectAccess: Pickler[ProjectAccess] =
    pickleMap[UserId.Public, ProjectPerm].xmap(ProjectAccess.apply)(_.value)

  implicit lazy val picklerProjectEvents: Pickler[ProjectEvents] =
    transformPickler(ProjectEvents.apply)(_.events)

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

  implicit lazy val picklerClientSideProjectEncryptionKey: Pickler[ClientSideProjectEncryptionKey] =
    transformPickler(ClientSideProjectEncryptionKey.apply)(_.value)

  implicit lazy val picklerProjectPerm: Pickler[ProjectPerm] =
    new Pickler[ProjectPerm] {
      private[this] final val KeyAdmin        = 0
      private[this] final val KeyCollaborator = 1
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
        val perm          = state.unpickle[ProjectPerm]
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

}
