package com.beardedlogic.usecase
package snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import scalaz.{-\/,\/-}

import app.AppSiteMap
import db.CreateProjectResult
import com.beardedlogic.usecase.lib.{InputValidator, SingleOpStatefulSnippet}
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
    InputValidator.projectName.correctAndValidate(projectNameInput) match {
      case -\/(err) => jsShowError(err)
      case \/-(name) =>
        daoProvider.withSession(_.createProject(currentUserId_!, name)) match {
          case Success(id)      => redirectTo(AppSiteMap.Project)(id)
          case NameAlreadyInUse => jsShowError("You already have a project with that name.")
        }
    }
  }
}
