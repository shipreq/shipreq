package shipreq.webapp.server.snippet

import java.time.Instant
import net.liftweb.util.Helpers._
import scalaz.\/-
import shipreq.taskman.api.UserId
import shipreq.webapp.base.data.{Project, ProjectCatalogue}
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol.{CreateProjectFn, InitDataForHomeSpa}
import shipreq.webapp.base.server.ApplyNewEvent
import shipreq.webapp.server.data.ProjectId
import shipreq.webapp.server.db.DaoT
import shipreq.webapp.server.db.EventDao.EventSeq
import shipreq.webapp.server.lib.SnippetHelpers
import shipreq.webapp.server.protocol.{ClientFn, ServerProtocol}

object HomeSpa extends SnippetHelpers {

  val InitProjectEvent = ProjectTemplateApply(ProjectTemplate.Default)
  val InitProject      = ApplyNewEvent.mustApply(InitProjectEvent, Project.empty)

  def createProject(d: DaoT, u: UserId, name: String): ProjectCatalogue.Item = {
    val projectId = d.createProject(u)

    val e1 = ApplyNewEvent.mustApply(ProjectNameSet(name), InitProject.project)
    d.createEvent(projectId, EventSeq(0), InitProject.ae, InitProject.ve.hashRecs)
    d.createEvent(projectId, EventSeq(1), e1.ae, e1.ve.hashRecs)

    ProjectCatalogue.Item(ProjectId Extern projectId, name, 0, 0, Instant.now(), None)
  }

  def render = {
    val user = currentUser_!()

    val projects = daoProvider.withSession(_.getProjectCatalogue(user.id))

    val createProjectFn = ServerProtocol.remoteFn(CreateProjectFn)(name =>
      \/-(daoProvider.withTransaction(createProject(_, user.id, name))))

    val data = InitDataForHomeSpa(
      user.username, projects, createProjectFn)

    "*" #> ClientFn.HomeSpa.runOnLoadHtml(data)
  }
}
