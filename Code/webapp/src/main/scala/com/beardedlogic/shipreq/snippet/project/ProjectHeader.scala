package shipreq.webapp
package snippet.project

import net.liftweb.http.js.JsCmd
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._

import app.{AppSiteMap, RequestVars}
import db.UpdateProjectResult._
import feature.validation.Validator
import lib.{NoticeFlash, SingleOpStatefulSnippet}
import lib.Types._
import snippet.DynModal
import util.HtmlTransformExt.ajaxSubmitOnClick
import util.JsExt.JsTextTrigger
import util.NonEmptyTemplate
import AppSiteMap.Implicits._
import db.AsyncDb

object ProjectHeaderConsts {
  val TriggerProjectUpdated = JsTextTrigger("project-updated")

  private val deleteModalBodyTemplate = NonEmptyTemplate.load("templates-hidden/modal_body-delete_project").get
  def DeleteModalBody(projectName: String) = {
    val t = ":text [data-confirmation]" #> projectName
    t(deleteModalBodyTemplate)
  }
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
  @inline final def pid = project.id
  @inline final def name = project.name
  var projectNameInput = name

  def render = (
    "#project-title" #> (
      "h1 *" #> name
      & "input .title [value]" #> name
      & "input .title" #> SHtml.onSubmit(projectNameInput = _)
      & "button .update" #> ajaxSubmitOnClick(onRename)
    )
    & ".readucs a [href]" #> AppSiteMap.ReadOwnUcs.relativeUrl(project)
    & ".delete" #> DynModal.confirmDangerT("project-del", Some(name), DeleteModalBody(name), None)(onDelete)
  )

  def onRename(): JsCmd =
    ifValid(Validator.projectName.correctAndValidate(projectNameInput))(newName =>
      daoProvider.withSession(_.updateProject(project.id, currentUserId_!, newName)) match {
        case DbSuccess        => jsRenamed(newName)
        case NameAlreadyInUse => jsShowError("You already have a project with that name.")
        case ProjectNotFound  => redirectHome
      }
    )

  def jsRenamed(newName: String): JsCmd = (
    jsClearError
    & jsShowNotice("Project renamed successfully.")
    & TriggerProjectUpdated.trigger(newName)
  )

  def onDelete(): Nothing = {
    daoProvider.withSession(_ deleteProjectSoft pid)
    asyncDb ! AsyncDb.DeleteProject(pid)
    NoticeFlash.notices.addS(s"Deleted Project: $name")
    redirectHome
  }
}
