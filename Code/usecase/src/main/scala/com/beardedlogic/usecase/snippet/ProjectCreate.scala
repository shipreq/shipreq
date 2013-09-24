package com.beardedlogic.usecase.snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Helpers._

import com.beardedlogic.usecase.app.AppSiteMap
import com.beardedlogic.usecase.db.CreateProjectResult
import com.beardedlogic.usecase.lib.SingleOpStatefulSnippet
import com.beardedlogic.usecase.util.HtmlTransformExt.ajaxSubmitOnClick
import AppSiteMap.Implicits._

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
      case Success(id)      => S.redirectTo(AppSiteMap.Project.relativeUrl(id))
      case InvalidName      => jsShowError("Invalid project name.")
      case NameAlreadyInUse => jsShowError("You already have a project with that name.")
    }
  }
}
