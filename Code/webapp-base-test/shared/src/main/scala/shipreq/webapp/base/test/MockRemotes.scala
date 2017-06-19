package shipreq.webapp.base.test

import java.time.Instant
import java.time.temporal.ChronoUnit
import shipreq.webapp.base.data.{ExternalId, Project, ProjectMetaData, Username}
import shipreq.webapp.base.protocol._

object MockRemotes {

  lazy val createProjectFn = ServerSideProc("CreateProject", HomeSpaProtocols.CreateProject)

  def mockUsername = Username("testuser")

  def projectSpa(p: Project): ProjectSpaProtocols.InitData =
    projectSpa(p, mockUsername)

  def projectSpa(p: Project, username: Username): ProjectSpaProtocols.InitData = {
    val now = Instant.now()
    val pi = ProjectMetaData(
      ExternalId("test"),
      p.name,
      1000,
      p.reqs.size,
      now.minus(99, ChronoUnit.DAYS),
      Some(now.minus(32, ChronoUnit.HOURS)))
    projectSpa(pi, username)
  }

  def projectSpa(p: ProjectMetaData): ProjectSpaProtocols.InitData =
    projectSpa(p, mockUsername)

  def projectSpa(p: ProjectMetaData, username: Username): ProjectSpaProtocols.InitData = {
    import ProjectSpaProtocols._
    ProjectSpaProtocols.InitData(
      username,
      p,
      ServerSideProc("initAsync"     , InitAsync            ),
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
