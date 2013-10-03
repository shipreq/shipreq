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
    else (
      ".none" #> "" &
      "li *" #> ps.map(p =>
        ".title *" #> p.name &
        "a [href]" #> AppSiteMap.Project.relativeUrl(p.id) &
        ".uc .qty" #> useCaseCount(p.ucCount) &
        useCaseLastMod(p.ucUpdatedAt)
      )
    )

  def useCaseCount(count: Int): String = count match {
    case 1 => "1 Use Case."
    case _ => s"$count Use Cases."
  }

  def useCaseLastMod(whenO: Option[String]) = whenO match {
    case None => ".uc .mod" #> ""
    case Some(when) => ".uc .mod abbr [title]" #> when
  }
}
