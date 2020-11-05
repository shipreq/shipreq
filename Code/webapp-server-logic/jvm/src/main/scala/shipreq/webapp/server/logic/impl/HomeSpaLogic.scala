package shipreq.webapp.server.logic.impl

import scalaz.syntax.monad._
import scalaz.{Monad, ~>}
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data._
import shipreq.webapp.member.data._
import shipreq.webapp.member.event._
import shipreq.webapp.member.protocol.ajax.HomeSpaProtocols
import shipreq.webapp.member.protocol.entrypoint.HomeSpaEntryPoint
import shipreq.webapp.server.logic.effect.DB
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

  def createProject[D[_]](userId: UserId,
                          name: Project.Name)
                         (implicit db: DB.ForHomeSpa[D], D: Monad[D]): D[ProjectMetaData] = {

    val e2 = ProjectNameSet(name)
    val p2 = ApplyNewEvent.mustApply(e2, InitProject.project).project
    val events = InitProjectEventV :+ e2

    for {
      pid <- db.createProject(userId, events, p2)
      pmd <- db.getProjectMetaData(pid)
    } yield pmd.get
  }

  def apply[D[_], F[_]](implicit db: DB.ForHomeSpa[D],
                        am: AssetManifest,
                        runDB: D ~> F,
                        D: Monad[D],
                        F: Monad[F]): HomeSpaLogic[F] =
    new HomeSpaLogic[F] {

      override def initData(user: User): F[HomeSpaEntryPoint.InitData] =
        for {
          p <- runDB(db.getAllProjectMetaDataForUser(user.id))
        } yield HomeSpaEntryPoint.InitData(user.username, p, am)

      override val ajaxCreateProject =
        (user, name) => runDB(createProject(user.id, name))
    }
}
