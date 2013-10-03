package com.beardedlogic.usecase
package snippet

import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._

import app.AppSiteMap
import db.{UseCaseHeader, UseCaseSummary}
import lib.{Misc, SingleOpStatefulSnippet}
import util.NonEmptyTemplate
import util.HtmlTransformExt.ajaxSubmitOnClick
import util.JsExt.JsHtmlTrigger
import lib.Types._
import AppSiteMap.Implicits._

object UseCaseCrudlConsts {
  val ListItemTemplate = NonEmptyTemplate.load("loggedin/project").quickExtract("template-li")

  final val TriggerCreated = JsHtmlTrigger("usecase-created")
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

  def renderList(ucs: List[UseCaseSummary]) = "li *" #> ucs.map(renderListItem)

  def renderListItem(uc: UseCaseSummary) = (
    "a .title" #> (
      "* *" #> s"UC-${uc.number}: ${uc.title}" &
      "* [href]" #> AppSiteMap.UseCaseEditor.relativeUrl(uc.parseId.get)
    ) &
    ".detail abbr [title]" #> uc.updatedAt
  )

  // -------------------------------------------------------------------------------------------------------------------
  // Create

  private var createTitle = ""

  def renderCreate = "#usecase-new" #> (
    ":text" #> SHtml.onSubmit(createTitle = _) &
      ":submit" #> ajaxSubmitOnClick(onCreate)
    )

  def onCreate(): JsCmd = {
    val h = UseCaseHeader(createTitle)
    // TODO createUseCaseIdentAndRev1 never returns an error
    val ucRev = daoProvider.withTransaction(_.createUseCaseIdentAndRev1(projectId, h))
    val ucs = new UseCaseSummary(ucRev, Misc.currentTimeAsIso8601Str)
    val li = renderListItem(ucs)(ListItemTemplate)
    TriggerCreated.trigger(li)
  }

}
