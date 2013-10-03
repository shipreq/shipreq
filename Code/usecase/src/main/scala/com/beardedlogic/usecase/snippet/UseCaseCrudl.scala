package com.beardedlogic.usecase
package snippet

import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.util.Helpers._

import app.AppSiteMap
import db.{UseCaseRev, UseCaseHeaderUpdateResult, UseCaseHeader, UseCaseSummary}
import lib.{ExternalId, Misc, SingleOpStatefulSnippet}
import util.NonEmptyTemplate
import util.HtmlTransformExt.ajaxSubmitOnClick
import util.JsExt.{JsTextTrigger, JqExpr, JsJsonTrigger, JsHtmlTrigger}
import lib.Types._
import AppSiteMap.Implicits._

object UseCaseCrudlConsts {
  val ListItemTemplate = NonEmptyTemplate.load("loggedin/project").quickExtract("template-li")

  final val TriggerCreated = JsHtmlTrigger("usecase-created")

  final val TriggerUpdated = JsJsonTrigger[UpdateDTO]("usecase-updated")
  case class UpdateDTO(eid: UseCaseIdentEI, li: JqExpr)

  final val TriggerUpdateNop = JsTextTrigger("usecase-update-nop")
}

/**
 * [C] Allows user to create a new use case.
 * [R] -
 * [U] Allows user to rename use cases.
 * [D] Pending.
 * [L] Displays a list of a project's use cases.
 *
 * @since 03/10/2013
 */
class UseCaseCrudl(projectId: ProjectId) extends SingleOpStatefulSnippet {
  import UseCaseCrudlConsts._

  def render = {
    val ucs = daoProvider.withSession(_.summariseUseCases(projectId))
    renderCreate & renderList(ucs)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // List

  def renderList(ucs: List[UseCaseSummary]) = "li" #> ucs.map(renderListItem)

  def renderListItem(uc: UseCaseSummary) = (
    "li [class+]" #> uc.eid &
    "a .title" #> (
      "* *" #> s"UC-${uc.number}: ${uc.title}" &
      "* [href]" #> AppSiteMap.UseCaseEditor.relativeUrl(uc.id)
    ) &
    ".detail abbr [title]" #> uc.updatedAt &
    renderEditItem(uc)
  )

  // -------------------------------------------------------------------------------------------------------------------
  // Create

  private var createTitle = ""

  def renderCreate = "#usecase-new" #> (
    ":text" #> SHtml.onSubmit(createTitle = _) &
    ":submit" #> ajaxSubmitOnClick(onCreate)
  )

  def onCreate(): JsCmd = {
    val ucs = create(createTitle)
    val li = renderListItem(ucs)(ListItemTemplate)
    TriggerCreated.trigger(li)
  }

  def create(title: String): UseCaseSummary = {
    val h = UseCaseHeader(title)
    // TODO createUseCaseIdentAndRev1 never returns an error
    val ucRev = daoProvider.withTransaction(_.createUseCaseIdentAndRev1(projectId, h))
    new UseCaseSummary(ucRev, Misc.currentTimeAsIso8601Str)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Update

  def renderEditItem(uc: UseCaseSummary) = {
    var title = ""
    val updateFn = () => onUpdate(uc, title)
    ".edit-mode" #> (
      ":text [value]" #> uc.title &
      ":text" #> SHtml.onSubmit(title = _) &
      ":submit" #> ajaxSubmitOnClick(updateFn)
    )
  }

  def onUpdate(uc: UseCaseSummary, newTitle: String): JsCmd = {
    update(uc.id, newTitle) match {
      case None =>
        TriggerUpdateNop.trigger(uc.eid)
      case Some(ucRev) =>
        val ucs = new UseCaseSummary(ucRev, Misc.currentTimeAsIso8601Str)
        val li = renderListItem(ucs)(ListItemTemplate)
        val dto = UpdateDTO(uc.eid, JqExpr(li))
        TriggerUpdated.trigger(dto)
    }
  }

  def update(id: UseCaseIdentId, newTitle: String): Option[UseCaseRev] = {
    import UseCaseHeaderUpdateResult._
    daoProvider.withTransaction(_.updateUseCaseHeader(id, _.copy(title = newTitle))) match {
      case NewRevision(r)     => Some(r)
      case DirectUpdate(r)    => Some(r)
      case AlreadyUpToDate(r) => None
      case UseCaseNotFound    => redirectTo(AppSiteMap.Project)(projectId)
    }
  }
}
