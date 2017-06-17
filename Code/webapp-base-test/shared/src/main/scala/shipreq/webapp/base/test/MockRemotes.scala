package shipreq.webapp.base.test

import java.time.Instant
import java.time.temporal.ChronoUnit
import shipreq.webapp.base.data.{ExternalId, Project, ProjectCatalogue, Username}
import shipreq.webapp.base.protocol._

object MockRemotes {

  lazy val createProjectFn = ServerSideProc("CreateProject", HomeSpaProtocols.CreateProject)

  def mockUsername = Username("testuser")

  def projectSpa(p: Project): ProjectSpaProtocols.InitClient =
    projectSpa(p, mockUsername)

  def projectSpa(p: Project, username: Username): ProjectSpaProtocols.InitClient = {
    val now = Instant.now()
    val pi = ProjectCatalogue.Item(
      ExternalId("test"),
      p.name,
      1000,
      p.reqs.size,
      now.minus(99, ChronoUnit.DAYS),
      Some(now.minus(32, ChronoUnit.HOURS)))
    projectSpa(pi, username)
  }

  def projectSpa(p: ProjectCatalogue.Item): ProjectSpaProtocols.InitClient =
    projectSpa(p, mockUsername)

  def projectSpa(p: ProjectCatalogue.Item, username: Username): ProjectSpaProtocols.InitClient = {
    import ProjectSpaProtocols._
    ProjectSpaProtocols.InitClient(
      username,
      p,
      ServerSideProc("projectInit"   , ProjectInit          ),
      ServerSideProc("issueTypeCrud" , CustomIssueTypeCrud  ),
      ServerSideProc("reqTypeCrud"   , CustomReqTypeCrud    ),
      ServerSideProc("reqTypeImpMod" , ReqTypeImplicationMod),
      ServerSideProc("fieldMandMod"  , FieldMandatorinessMod),
      ServerSideProc("fieldCrud"     , FieldCrud.Protocol   ),
      ServerSideProc("tagCrud"       , TagCrud.Protocol     ),
      ServerSideProc("createContent" , CreateContent        ),
      ServerSideProc("updateContent" , UpdateContent        ),
      ServerSideProc("projectNameSet", ProjectNameSet       ))
  }
}
