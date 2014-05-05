package shipreq.webapp.snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import shipreq.webapp.app.AppSiteMap
import shipreq.webapp.db.CreateProjectResult._
import shipreq.webapp.feature.validation.Validators
import shipreq.webapp.lib.{FormVar, SingleOpStatefulSnippet}
import shipreq.webapp.util.HtmlTransformExt.ajaxSubmitOnClick

/**
 * Form to create a new project.
 *
 * @since 24/09/2013
 */
class ProjectCreate extends SingleOpStatefulSnippet {

  private[snippet] val nameV = FormVar.strOnSubmit(Validators.project.name, ":text")("")

  def render =
    nameV.csssel & ":submit" #> ajaxSubmitOnClick(onSubmit)

  def onSubmit(): JsCmd =
    ifValid(nameV.validate)(name =>
      daoProvider.withSession(_.createProject(currentUserId_!, name)) match {
        case DbSuccess(id)    => redirectTo(AppSiteMap.Project)(id)
        case NameAlreadyInUse => jsShowError("You already have a project with that name.")
      }
    )
}
