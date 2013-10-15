package com.beardedlogic.usecase
package snippet

import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.util.Helpers._
import scalaz.{\/, -\/, \/-}

import app.AppSiteMap
import db.{UseCaseRev, UseCaseHeaderUpdateResult, UseCaseHeader, UseCaseSummary2}
import lib.{InputValidator, Locks, ExternalId, Misc, SingleOpStatefulSnippet}
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
    val ucs = daoProvider.withSession(_.summariseUseCases2(projectId))
    renderCreate & renderList(ucs)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // List

  def renderList(ucs: List[UseCaseSummary2]) = "li" #> ucs.map(renderListItem)

  def renderListItem(uc: UseCaseSummary2) = (
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

  def onCreate(): JsCmd =
    create(createTitle) match {
      case -\/(err) => jsShowError(err)
      case \/-(ucs) => jsClearError & TriggerCreated.trigger(renderListItem(ucs)(ListItemTemplate))
    }

  def create(titleInput: String): String \/ UseCaseSummary2 =
    InputValidator.useCaseTitle.correctAndValidate(titleInput).map(newTitle => {
      val h = UseCaseHeader(newTitle)
      val ucRev = Locks.UseCaseNumbers.write(projectId)(lock =>
        daoProvider.withTransaction(_.createUseCaseIdentAndRev1(projectId, h, lock))
      )
      new UseCaseSummary2(ucRev, Misc.currentTimeAsIso8601Str)
    })

  // -------------------------------------------------------------------------------------------------------------------
  // Update

  def renderEditItem(uc: UseCaseSummary2) = {
    var title = ""
    val updateFn = () => onUpdate(uc, title)
    ".edit-mode" #> (
      ":text [value]" #> uc.title &
      ":text" #> SHtml.onSubmit(title = _) &
      ":submit" #> ajaxSubmitOnClick(updateFn)
    )
  }

  def onUpdate(uc: UseCaseSummary2, newTitle: String): JsCmd =
    update(uc.id, newTitle) match {
      case -\/(err)  => jsShowError(err)
      case \/-(None) => jsClearError & TriggerUpdateNop.trigger(uc.eid)
      case \/-(Some(ucRev)) =>
        val ucs = new UseCaseSummary2(ucRev, Misc.currentTimeAsIso8601Str)
        val li = renderListItem(ucs)(ListItemTemplate)
        val dto = UpdateDTO(uc.eid, JqExpr(li))
        jsClearError & TriggerUpdated.trigger(dto)
    }

  def update(id: UseCaseIdentId, titleInput: String): String \/ Option[UseCaseRev] = {
    import UseCaseHeaderUpdateResult._
    InputValidator.useCaseTitle.correctAndValidate(titleInput).map(newTitle =>
      Locks.SingleUseCase.write(id, projectId)(lock =>
        daoProvider.withTransaction(_.updateUseCaseHeader(id, _.copy(title = newTitle), lock))
      ) match {
        case Success(r)         => Some(r)
        case AlreadyUpToDate(r) => None
        case UseCaseNotFound    => redirectTo(AppSiteMap.Project)(projectId)
      })
  }
}
