package com.beardedlogic.usecase
package snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._

import db.UpdateProjectResult
import lib.SingleOpStatefulSnippet
import lib.Types._
import security.PermissionCheck
import util.HtmlTransformExt.ajaxSubmitOnClick
import util.JsExt.JsTextTrigger
import app.RequestVars

private[snippet] object ProjectHeaderConsts {
  final val TriggerProjectUpdated = JsTextTrigger("project-updated")
}

/**
 * Renders the header on the project page.
 *
 * @since 30/09/2013
 */
class ProjectHeader extends SingleOpStatefulSnippet {
  import ProjectHeaderConsts._
  implicit def alertId = "phdra".tag[AlertIdTag]

  val project = RequestVars.SoleProject.get
  var projectName = project.name

  def render = (
    "#project-title" #> (
      "h1 *" #> project.name &
      "input .title [value]" #> project.name &
      "input .title" #> SHtml.onSubmit(projectName = _) &
      "button .update" #> ajaxSubmitOnClick(onRename)
    )
  )

  def onRename(): JsCmd = {
    import UpdateProjectResult._
    daoProvider.withSession(_.updateProject(project.id, currentUserId_!, projectName)) match {
      case Success(newName) => jsRenamed(newName)
      case InvalidName      => jsShowError("Invalid project name.")
      case NameAlreadyInUse => jsShowError("You already have a project with that name.")
      case ProjectNotFound  => redirectHome
    }
  }

  def jsRenamed(newName: String): JsCmd = (
    jsClearError
    & jsShowAlertSuccess("Project renamed successfully.")
    & TriggerProjectUpdated.trigger(newName)
  )
}
