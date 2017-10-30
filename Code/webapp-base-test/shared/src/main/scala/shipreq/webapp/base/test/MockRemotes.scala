package shipreq.webapp.base.test

import shipreq.webapp.base.data._
import shipreq.webapp.base.user._
import shipreq.webapp.base.protocol._
import UnsafeTypes._

object MockRemotes {

  lazy val createProjectFn = ServerSideProc("CreateProject", HomeSpaProtocols.CreateProject)

  def mockUsername = Username("testuser")

  def projectSpa(p: Project                    ): ProjectSpaProtocols.InitData = projectSpa(p, mockUsername)
  def projectSpa(p: Project.Name               ): ProjectSpaProtocols.InitData = projectSpa(p, mockUsername)
  def projectSpa(p: Project, username: Username): ProjectSpaProtocols.InitData = projectSpa(p.name, username)

  def projectSpa(p: Project.Name, username: Username): ProjectSpaProtocols.InitData = {
    import ProjectSpaProtocols._
    ProjectSpaProtocols.InitData(
      username,
      p,
      ServerSideProc("initAsync"       , InitAsync            ),
      ServerSideProc("issueTypeCrud"   , CustomIssueTypeCrud  ),
      ServerSideProc("reqTypeCrud"     , CustomReqTypeCrud    ),
      ServerSideProc("reqTypeImpMod"   , ReqTypeImplicationMod),
      ServerSideProc("fieldMandMod"    , FieldMandatorinessMod),
      ServerSideProc("fieldCrud"       , FieldCrud.Protocol   ),
      ServerSideProc("tagCrud"         , TagCrud.Protocol     ),
      ServerSideProc("createContent"   , CreateContent        ),
      ServerSideProc("updateContent"   , UpdateContent        ),
      ServerSideProc("updateSavedViews", UpdateSavedViews     ),
      ServerSideProc("projectNameSet"  , ProjectNameSet       ))
  }
}
