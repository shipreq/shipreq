package shipreq.webapp.base

import shipreq.webapp.base.data.Project

@deprecated("This should be synced with Lift", "")
object URLs {

  def PageLogout = "/logout"

  def PageMemberHome = "/"

  def PageProject(id: Project.XId): String =
    "/project/" + id.value
}
