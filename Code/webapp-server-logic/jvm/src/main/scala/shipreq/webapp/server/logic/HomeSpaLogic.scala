package shipreq.webapp.server.logic

import java.time.Instant
import scalaz.syntax.monad._
import scalaz.{Monad, \/-, ~>}
import shipreq.webapp.base.data.{Project, ProjectMetaData}
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol.HomeSpaProtocols
import shipreq.webapp.base.user._
import Event._

trait HomeSpaLogic[F[_]] extends HomeSpaLogic.Ajax[F] {
  def initData(user: User): F[HomeSpaProtocols.InitData]
}

object HomeSpaLogic {

  trait Ajax[F[_]] {
    val ajaxCreateProject: HomeSpaProtocols.createProject.ServerSideFnI[F, User]
  }

  val InitProjectEvent = ProjectTemplateApply(ProjectTemplate.default)
  val InitProject      = ApplyNewEvent.mustApply(InitProjectEvent, Project.empty)

  def createProject[D[_]](userId: UserId,
                          name: Project.Name,
                          now: Instant)
                         (implicit db: DB.ForHomeSpa[D], D: Monad[D]): D[ProjectMetaData] = {

    val e1 = DB.SaveProjectEventCmd(EventOrd.first, InitProject.event, InitProject.hashRecs)
    val u2 = ApplyNewEvent.mustApply(ProjectNameSet(name), InitProject.project)
    val e2 = DB.SaveProjectEventCmd(e1.ord.next, u2.event, u2.hashRecs)
    val ecount = 2
    val events = e1 :: e2 :: Nil

    db.inDbTransaction(
      for {
        pid ← db.createEmptyProject(userId, ecount)
        r   ← db.saveProjectEvents(pid, events)
        _   = r.leftMap(throw _) // For unit tests - should be impossible
      } yield ProjectMetaData(Obfuscators.projectId.obfuscate(pid), name, ecount, ecount, 0, now, None))
  }

  def apply[D[_], F[_]](implicit db: DB.ForHomeSpa[D],
                        runDB: D ~> F,
                        svr: Server.Algebra[F],
                        D: Monad[D],
                        F: Monad[F]): HomeSpaLogic[F] =
    new HomeSpaLogic[F] {

      override def initData(user: User): F[HomeSpaProtocols.InitData] =
        for {
          p <- runDB(db.getAllProjectMetaDataForUser(user.id))
        } yield HomeSpaProtocols.InitData(user.username, p)

      override val ajaxCreateProject =
        (user, name) => svr.now.flatMap(now => runDB(createProject(user.id, name, now)))
    }
}
