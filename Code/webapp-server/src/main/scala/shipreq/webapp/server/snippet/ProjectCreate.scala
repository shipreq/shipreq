package shipreq.webapp.server.snippet

import scalaz.{\/, -\/, \/-}
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import shipreq.taskman.api.UserId
import shipreq.webapp.base.event.{ApplyTemplate, ProjectTemplate}
import shipreq.webapp.server.app.AppSiteMap
import shipreq.webapp.server.db.CreateProjectResult._
import shipreq.webapp.server.db.DaoT
import shipreq.webapp.server.db.EventDao.EventSeq
import shipreq.webapp.server.feature.validation.Validators
import shipreq.webapp.server.lib.{FormVar, SingleOpStatefulSnippet}
import shipreq.webapp.server.lib.Types.ProjectId
import shipreq.webapp.server.util.HtmlTransformExt.ajaxSubmitOnClick

/**
 * Form to create a new project.
 *
 * @since 24/09/2013
 */
object ProjectCreate extends SingleOpStatefulSnippet {

  val form = FormVar.strOnSubmit(Validators.project.name, ":text")

  def render = {
    var vars: form.Var = ""
    form.csssel(vars, vars = _) &
      ":submit" #> ajaxSubmitOnClick(() => onSubmit(vars))
  }

  def onSubmit(vars: form.Var): JsCmd =
    ifValid(form validate vars) { name =>
      val uid = currentUserId_!()
      daoProvider.withTransaction(create(_, uid, name)) match {
        case \/-(id) => redirectTo(AppSiteMap.Project)(id)
        case -\/(err) => jsShowError(err)
      }
    }

  private def create(dao: DaoT, userId: UserId, name: String): String \/ ProjectId =
    dao.createProject(userId, name) match {

      case DbSuccess(id) =>
        val t = ProjectTemplate.Default
        dao.createEvent(id, EventSeq(0), ApplyTemplate(t), t.hashRecs)
        \/-(id)

      case NameAlreadyInUse =>
        -\/("You already have a project with that name.")
    }
}
