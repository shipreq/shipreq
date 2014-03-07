package shipreq.webapp
package snippet.project

import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._

import app.{RequestVars, AppSiteMap}
import db.{UseCaseRev, UseCaseHeaderUpdateResult, UseCaseHeader, UseCaseSummary}
import feature.validation.Validator
import lib.{Locks, Misc, SingleOpStatefulSnippet}
import lib.Types._
import util.NonEmptyTemplate
import util.HtmlTransformExt.ajaxSubmitOnClick
import util.JsExt.{JsTextTrigger, JqExpr, JsJsonTrigger, JsHtmlTrigger}
import AppSiteMap.Implicits._

object UseCaseCrudlConsts {
  val ListItemTemplate = NonEmptyTemplate.load("loggedin/project").quickExtractById("template-li")

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
    val ucs = RequestVars.UseCases.get
    renderCreate & renderList(ucs)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // List

  def renderList(ucs: List[UseCaseSummary]) = "li" #> ucs.map(renderListItem)

  def renderListItem(uc: UseCaseSummary) = (
    "li [class+]" #> uc.eid &
    "a .title" #> (
      "* *" #> uc.fullName &
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
    ifValid(create(createTitle))(ucs =>
      jsClearError & TriggerCreated.trigger(renderListItem(ucs)(ListItemTemplate)))

  def create(titleInput: String): ValidationResultU[UseCaseSummary] =
    Validator.useCaseTitle.correctAndValidate(titleInput).map(newTitle => {
      val h = UseCaseHeader(newTitle)
      val ucRev = Locks.UseCaseNumbers.write(projectId)(lock =>
        daoProvider.withTransaction(_.createUseCaseIdentAndRev1(projectId, h, lock)))
      new UseCaseSummary(ucRev, Misc.currentTimeAsIso8601Str)
    })

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

  def onUpdate(uc: UseCaseSummary, newTitle: String): JsCmd =
    ifValid(update(uc.id, newTitle)){
      case None =>
        jsClearError & TriggerUpdateNop.trigger(uc.eid)
      case Some(ucRev) =>
        val ucs = new UseCaseSummary(ucRev, Misc.currentTimeAsIso8601Str)
        val li = renderListItem(ucs)(ListItemTemplate)
        val dto = UpdateDTO(uc.eid, JqExpr(li))
        jsClearError & TriggerUpdated.trigger(dto)
    }

  def update(id: UseCaseIdentId, titleInput: String): ValidationResultU[Option[UseCaseRev]] = {
    import UseCaseHeaderUpdateResult._
    Validator.useCaseTitle.correctAndValidate(titleInput).map(newTitle =>
      Locks.SingleUseCase.write(id, projectId)(lock =>
        daoProvider.withTransaction(_.updateUseCaseHeader(id, _.copy(title = newTitle), lock))
      ) match {
        case DbSuccess(r)       => Some(r)
        case AlreadyUpToDate(r) => None
        case UseCaseNotFound    => redirectTo(AppSiteMap.Project)(projectId)
      })
  }
}
