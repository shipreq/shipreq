package com.beardedlogic.usecase
package snippet.project

import net.liftweb.http.js.JsCmd
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._

import app.{AppSiteMap, RequestVars}
import db.UpdateProjectResult._
import feature.validation.Validator
import lib.SingleOpStatefulSnippet
import lib.Types._
import util.HtmlTransformExt.ajaxSubmitOnClick
import util.JsExt.JsTextTrigger
import AppSiteMap.Implicits._

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
  override implicit def errorAlertId = "phdra".tag

  val project = RequestVars.Project.get.value
  var projectNameInput = project.name

  def render = (
    "#project-title" #> (
      "h1 *" #> project.name &
      "input .title [value]" #> project.name &
      "input .title" #> SHtml.onSubmit(projectNameInput = _) &
      "button .update" #> ajaxSubmitOnClick(onRename)
    ) &
    "a .readucs [href]" #> AppSiteMap.ReadOwnUcs.relativeUrl(project)
  )

  def onRename(): JsCmd =
    ifValid(Validator.projectName.correctAndValidate(projectNameInput))(name =>
      daoProvider.withSession(_.updateProject(project.id, currentUserId_!, name)) match {
        case DbSuccess        => jsRenamed(name)
        case NameAlreadyInUse => jsShowError("You already have a project with that name.")
        case ProjectNotFound  => redirectHome
      }
    )

  def jsRenamed(newName: String): JsCmd = (
    jsClearError
    & jsShowNotice("Project renamed successfully.")
    & TriggerProjectUpdated.trigger(newName)
  )
}
