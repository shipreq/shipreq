package shipreq.webapp.server.logic.logic

import cats.syntax.all._
import cats.{Monad, ~>}
import shipreq.base.util.CatsExtra._
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data._
import shipreq.webapp.member.global.GlobalEvent
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.protocol.ajax.HomeSpaProtocols
import shipreq.webapp.member.protocol.entrypoint.HomeSpaEntryPoint
import shipreq.webapp.server.logic.algebra.{Crypto, DB}
import shipreq.webapp.server.logic.data.ProjectEncryptionKey
import shipreq.webapp.server.logic.event.ApplyNewEvent
import shipreq.webapp.server.logic.util.Obfuscators

trait HomeSpaLogic[F[_]] extends HomeSpaLogic.Ajax[F] {
  def initData(user: User): F[HomeSpaEntryPoint.InitData]
}

object HomeSpaLogic {
  import Event._

  trait Ajax[F[_]] {
    val ajaxCreateProject  : HomeSpaProtocols.CreateProject  .ajax.ServerSideFnI[F, User]
    val ajaxCreateUserGroup: HomeSpaProtocols.CreateUserGroup.ajax.ServerSideFnI[F, User]
    val ajaxUpdateUserGroup: HomeSpaProtocols.UpdateUserGroup.ajax.ServerSideFnI[F, User]
  }

  val InitProjectEvent  = ProjectTemplateApply(ProjectTemplate.default)
  val InitProject       = ApplyNewEvent.mustApply(InitProjectEvent, Project.empty)
  val InitProjectEventV = Vector.empty[ActiveEvent] :+ InitProject.event

  def createProject[D[_]](userId    : UserId,
                          name      : Project.Name)
                         (implicit D: Monad[D],
                          db        : DB.ForHomeSpa[D],
                          crypto    : Crypto[D]): D[ProjectMetaData] = {

    val e2 = ProjectNameSet(name)

    // It's ok to use projectPartial and not worry about updating history here because:
    //
    // 1. History never affects subsequent event application
    // 2. db.createProject only needs a Project instance to extract a few stats for the header record
    val p2 = ApplyNewEvent.mustApply(e2, InitProject.projectPartial).projectPartial

    val events = InitProjectEventV :+ e2

    for {
      key <- crypto.generateKey256
      pid <- db.createProject(userId, events, p2, ProjectEncryptionKey(key))
      pmd <- db.getProjectMetaData(pid)
    } yield pmd.get
  }

  // ===================================================================================================================

  def apply[D[_], F[_]](implicit db: DB.ForHomeSpa[D],
                        am: AssetManifest,
                        crypto: Crypto[D],
                        runDB: D ~> F,
                        D: Monad[D]): HomeSpaLogic[F] = new HomeSpaLogic[F] {

    import HomeSpaProtocols._

    override def initData(user: User): F[HomeSpaEntryPoint.InitData] = {
      val load: D[HomeSpaEntryPoint.InitData] =
        for {
          projects   <- db.getAllProjectMetaDataForUser(user.id)
          userGroups <- db.getUserGroupUniverseForUser(user.id)
        } yield {
          val obfuscateId = Obfuscators.userGroupId.obfuscate
          val userGroups2 = userGroups.xmap(userGroups.users.apply, _ => (), obfuscateId, _ mapId obfuscateId)
          HomeSpaEntryPoint.InitData(user.username, projects, userGroups2, am)
        }
      db.inStrictTxn(runDB)(load)
    }

    override val ajaxCreateProject =
      (user, name) => runDB(createProject[D](user.id, name))

    override val ajaxCreateUserGroup =
      (user, req) => {

        def createWith(getUserId: Username => UserId): D[CreateUserGroup.Response] = {
          val rels = req.rels.xmap(Obfuscators.userGroupId.deobfuscateOrThrow(_), getUserId)
          for {
            res <- db.createUserGroup(req.name, req.handle, rels)
            _   <- db.logGlobalEventOnRight(res)(GlobalEvent.UserGroupCreate(user.id, _, req.name, req.handle, rels))
          } yield res match {
            case \/-(id)  => CreateUserGroup.Response.Success(Obfuscators.userGroupId.obfuscate(id))
            case -\/(err) => CreateUserGroup.Response.SaveError(err.map(Obfuscators.userGroupId.obfuscate))
          }
        }

        val create: D[CreateUserGroup.Response] =
          db.getUserIdsByUsername(req.usernames).flatMap {
            case \/-(m)    => createWith(m.apply)
            case -\/(errs) => D pure CreateUserGroup.Response.InvalidUsernames(errs)
          }

        db.inStrictTxn(runDB)(create)
      }

    override val ajaxUpdateUserGroup =
      (user, req) => {

        val id = Obfuscators.userGroupId.deobfuscateOrThrow(req.id)

        def updateWith(getUserId: Username => UserId): D[UpdateUserGroup.Response] = {
          val rels  = req.rels.xmap(Obfuscators.userGroupId.deobfuscateOrThrow(_), getUserId)
          for {
            res <- db.updateUserGroup(user.id, id, req.name, req.handle, rels)
            _   <- db.logGlobalEventOnRight(res)(_ => GlobalEvent.UserGroupUpdate(user.id, id, req.name, req.handle, rels))
          } yield res match {
            case \/-(_)   => UpdateUserGroup.Response.Success
            case -\/(err) => UpdateUserGroup.Response.SaveError(err.map(Obfuscators.userGroupId.obfuscate))
          }
        }

        val update: D[UpdateUserGroup.Response] =
          db.getUserIdsByUsername(req.usernames).flatMap {
            case \/-(m)    => updateWith(m.apply)
            case -\/(errs) => D pure UpdateUserGroup.Response.InvalidUsernames(errs)
          }

        db.inStrictTxn(runDB)(update)
      }
  }
}
