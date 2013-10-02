package com.beardedlogic.usecase
package snippet.project

import net.liftweb.http.js.JsCmd
import net.liftweb.http.{SHtml, StatefulSnippet}
import net.liftweb.util.Helpers._

import db.UpdateProjectResult
import lib.SnippetHelpers
import lib.Types._
import util.HtmlTransformExt.ajaxSubmitOnClick
import util.JsExt.JsTextTrigger

private[project] object HeaderConsts {
  final val TriggerProjectUpdated = JsTextTrigger("project-updated")
}

/**
 * Renders the header on the project page.
 *
 * @since 30/09/2013
 */
class header(projectId: ProjectId) extends StatefulSnippet with SnippetHelpers {
  import HeaderConsts._

  override def dispatch = { case _ => render }

  // TODO would this be better using Shiro's authorisation?
  val project = requireResult_!(for {
    dao <- daoProvider.forSession
    p   <- dao.findProject(projectId) if p.owner == currentUserId_!
  } yield p)

  private[snippet] var projectName = project.name

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
    daoProvider.withSession(_.updateProject(projectId, currentUserId_!, projectName)) match {
      case Success(newName) => jsRenamed(newName)
      case InvalidName      => jsShowError("Invalid project name.")
      case NameAlreadyInUse => jsShowError("You already have a project with that name.")
      case ProjectNotFound  => redirectHome
    }
  }

  def jsRenamed(newName: String): JsCmd = (
    jsClearError()
    & jsShowAlertSuccess("Project renamed successfully.")
    & TriggerProjectUpdated.trigger(newName)
  )
}
