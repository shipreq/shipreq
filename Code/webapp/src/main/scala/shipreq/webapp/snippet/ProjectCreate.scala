package shipreq.webapp
package snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import scalaz.{Failure, Success}

import app.AppSiteMap
import db.CreateProjectResult
import lib.SingleOpStatefulSnippet
import feature.validation.Validator
import util.HtmlTransformExt.ajaxSubmitOnClick

/**
 * Form to create a new project.
 *
 * @since 24/09/2013
 */
class ProjectCreate extends SingleOpStatefulSnippet {

  private[snippet] var projectNameInput = ""

  def render = (
    ":text" #> SHtml.onSubmit(projectNameInput = _) &
    ":submit" #> ajaxSubmitOnClick(onSubmit)
  )

  def onSubmit(): JsCmd = {
    import CreateProjectResult._
    Validator.project.name.correctAndValidate(projectNameInput) match {
      case Failure(f)    => jsShowFailure(f)
      case Success(name) =>
        daoProvider.withSession(_.createProject(currentUserId_!, name)) match {
          case DbSuccess(id)    => redirectTo(AppSiteMap.Project)(id)
          case NameAlreadyInUse => jsShowError("You already have a project with that name.")
        }
    }
  }
}
