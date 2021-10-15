package shipreq.webapp.server.logic.impl

import cats.syntax.all._
import cats.{Monad, ~>}
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.protocol.ajax.HomeSpaProtocols
import shipreq.webapp.member.protocol.entrypoint.HomeSpaEntryPoint
import shipreq.webapp.server.logic.algebra.{Crypto, DB}
import shipreq.webapp.server.logic.data.ProjectEncryptionKey
import shipreq.webapp.server.logic.event.ApplyNewEvent

trait HomeSpaLogic[F[_]] extends HomeSpaLogic.Ajax[F] {
  def initData(user: User): F[HomeSpaEntryPoint.InitData]
}

object HomeSpaLogic {
  import Event._

  trait Ajax[F[_]] {
    val ajaxCreateProject: HomeSpaProtocols.CreateProject.ajax.ServerSideFnI[F, User]
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

  def apply[D[_], F[_]](implicit db: DB.ForHomeSpa[D],
                        am: AssetManifest,
                        crypto: Crypto[D],
                        runDB: D ~> F,
                        D: Monad[D],
                        F: Monad[F]): HomeSpaLogic[F] =
    new HomeSpaLogic[F] {

      override def initData(user: User): F[HomeSpaEntryPoint.InitData] =
        for {
          p <- runDB(db.getAllProjectMetaDataForUser(user.id))
        } yield HomeSpaEntryPoint.InitData(user.username, p, am)

      override val ajaxCreateProject =
        (user, name) => runDB(createProject[D](user.id, name))
    }
}
