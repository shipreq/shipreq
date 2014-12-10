package shipreq.webapp.snippet.project

import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._

import shipreq.webapp.base.validation.ValidationResult
import shipreq.webapp.app.{RequestVars, AppSiteMap}
import shipreq.webapp.db.{UseCaseRev, UseCaseHeaderUpdateResult, UseCaseHeader, UseCaseSummary}
import shipreq.webapp.feature.validation.Validators
import shipreq.webapp.lib.{Locks, Misc, SingleOpStatefulSnippet}
import shipreq.webapp.lib.Types._
import shipreq.webapp.util.NonEmptyTemplate
import shipreq.webapp.util.HtmlTransformExt.ajaxSubmitOnClick
import shipreq.webapp.util.JsExt.{JsTextTrigger, JqExpr, JsJsonTrigger, JsHtmlTrigger}
import AppSiteMap.Implicits._

object UseCaseCrudlConsts {
  val ListItemTemplate = NonEmptyTemplate.load("loggedin/project").quickExtractById("template-li")

  final val TriggerCreated = JsHtmlTrigger("usecase-created")

  final val TriggerUpdated = JsJsonTrigger[UpdateDTO]("usecase-updated")
  case class UpdateDTO(eid: String, li: JqExpr) // TODO change back to UseCaseIdentIdE after agronaut

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
    "li [class+]" #> uc.eid.value &
    "a .title" #> (
      "* *" #> uc.fullName &
      "* [href]" #> AppSiteMap.UseCaseEditor.relativeUrl(uc.id)
    ) &
    ".detail abbr [title]" #> uc.updatedAt.value &
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
      TriggerCreated.trigger(renderListItem(ucs)(ListItemTemplate)))

  def create(titleInput: String): ValidationResult[UseCaseSummary] =
    Validators.usecase.title.correctAndValidateU(titleInput).map(newTitle => {
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
        TriggerUpdateNop.trigger(uc.eid)
      case Some(ucRev) =>
        val ucs = new UseCaseSummary(ucRev, Misc.currentTimeAsIso8601Str)
        val li = renderListItem(ucs)(ListItemTemplate)
        val dto = UpdateDTO(uc.eid, JqExpr(li))
        TriggerUpdated.trigger(dto)
    }

  def update(id: UseCaseIdentId, titleInput: String): ValidationResult[Option[UseCaseRev]] = {
    import UseCaseHeaderUpdateResult._
    Validators.usecase.title.correctAndValidateU(titleInput).map(newTitle =>
      Locks.SingleUseCase.write(id, projectId)(lock =>
        daoProvider.withTransaction(_.updateUseCaseHeader(id, _.copy(title = newTitle), lock))
      ) match {
        case DbSuccess(r)       => Some(r)
        case AlreadyUpToDate(r) => None
        case UseCaseNotFound    => redirectTo(AppSiteMap.Project)(projectId)
      })
  }
}
