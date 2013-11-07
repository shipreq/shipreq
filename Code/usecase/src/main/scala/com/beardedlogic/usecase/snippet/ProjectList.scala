package com.beardedlogic.usecase
package snippet

import net.liftweb.util.Helpers._
import app.AppSiteMap
import db.ProjectSummary
import lib.SnippetHelpers
import AppSiteMap.Implicits._

/**
 * Displays a list of a user's projects.
 *
 * @since 27/09/2013
 */
object ProjectList extends SnippetHelpers {

  def render = {
    val userId = currentUserId_!
    val ps = daoProvider.withSession(_.summariseProjects(userId))
    renderProjectList(ps)
  }

  def renderProjectList(ps: List[ProjectSummary]) =
    if (ps.isEmpty)
      "ol" #> ""
    else
      ".none" #> "" & "li *" #> ps.map(renderProject)

  def renderProject(p: ProjectSummary) = (
    ".title *" #> p.name
    & "a [href]" #> AppSiteMap.Project.relativeUrl(p.id)
    & ".uc" #> (
      ".c" #> useCaseCount(p.ucCount)
      & detailTimeSpan(p.ucUpdatedAt)
    )
    & ".shares" #> (
      ".c" #> shareCount(p.shareCount, p.shareViews)
      & detailTimeSpan(p.shareLastViewedAt)
    )
  )

  val useCaseCount = pluralise("use case.", "use cases.") _

  def shareCount(count: Long, views: Long): String = count match {
    case 0 => "0 shares."
    case 1 => s"1 share with ${shareViews(views)}"
    case _ => s"$count shares with ${shareViews(views)}"
  }

  val shareViews = pluralise("view.", "views.") _

  def detailTimeSpan(whenO: Option[String]) = whenO match {
    case None       => ".t" #> ""
    case Some(when) => ".t abbr [title]" #> when
  }
}
