package com.beardedlogic.usecase
package snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._

import app.AppSiteMap
import db.CreateProjectResult
import lib.SingleOpStatefulSnippet
import util.HtmlTransformExt.ajaxSubmitOnClick

/**
 * Form to create a new project.
 *
 * @since 24/09/2013
 */
class ProjectCreate extends SingleOpStatefulSnippet {

  private[snippet] var projectName = ""

  def render = {
    requireLogin_!
    ":text" #> SHtml.onSubmit(projectName = _) &
    ":submit" #> ajaxSubmitOnClick(onSubmit)
  }

  def onSubmit(): JsCmd = {
    import CreateProjectResult._
    daoProvider.withSession(_.createProject(currentUserId_!, projectName)) match {
      case Success(id)      => redirectTo(AppSiteMap.Project)(id)
      case InvalidName      => jsShowError("Invalid project name.")
      case NameAlreadyInUse => jsShowError("You already have a project with that name.")
    }
  }
}
