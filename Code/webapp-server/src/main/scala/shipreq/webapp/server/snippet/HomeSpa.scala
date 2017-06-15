package shipreq.webapp.server.snippet

import doobie.imports._
import java.time.Instant
import net.liftweb.util.Helpers._
import scalaz.\/-
import shipreq.taskman.api.UserId
import shipreq.base.db.DoobieHelpers._
import shipreq.webapp.base.data.{Project, ProjectCatalogue}
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol.{CreateProjectFn, InitDataForHomeSpa}
import shipreq.webapp.server.db.{DbLogic, EventSeq}
import shipreq.webapp.server.lib.SnippetHelpers
import shipreq.webapp.server.logic._
import shipreq.webapp.server.protocol.{ClientFn, ServerProtocol}

object HomeSpa extends SnippetHelpers {

  val InitProjectEvent = ProjectTemplateApply(ProjectTemplate.Default)
  val InitProject      = ApplyNewEvent.mustApply(InitProjectEvent, Project.empty)

  def createProject(u: UserId, name: String, now: Instant): ConnectionIO[ProjectCatalogue.Item] =
    (for {
      projectId <- DbLogic.project.create(u)
      e1 = ApplyNewEvent.mustApply(ProjectNameSet(name), InitProject.project)
      _ <- DbLogic.event.create(projectId, EventSeq(0), InitProject.ae, InitProject.ve.hashRecs)
      _ <- DbLogic.event.create(projectId, EventSeq(1), e1.ae, e1.ve.hashRecs)
    } yield ProjectCatalogue.Item(ProjectId Extern projectId, name, 0, 0, now, None)
    ).inTransaction

  def render = {
    val user = currentUser_!()

    val projects = db().io.trans(DbLogic.project.getCatalogue(user.id)).unsafePerformIO()

    val createProjectFn = ServerProtocol.remoteFn(CreateProjectFn)(name =>
      db().io.trans(createProject(user.id, name, Instant.now())).map(\/-(_)))

    val data = InitDataForHomeSpa(
      user.username, projects, createProjectFn)

    "*" #> ClientFn.HomeSpa.htmlToRunOnLoad(data)
  }
}
