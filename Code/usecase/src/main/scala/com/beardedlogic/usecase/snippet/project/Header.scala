package com.beardedlogic.usecase
package snippet.project

import net.liftweb.http.StatefulSnippet
import net.liftweb.util.Helpers._

import lib._
import Types._

/**
 * Renders the header on the project page.
 *
 * @since 30/09/2013
 */
class header(projectId: ProjectId) extends StatefulSnippet with SnippetHelpers {

  override def dispatch = { case _ => render }

  // TODO would this be better using Shiro's authorisation?
  val project = requireResult_!(for {
    dao <- daoProvider.forSession
    p   <- dao.findProject(projectId) if p.owner == currentUserId_!
  } yield p)

  def render = (
    "h1 *" #> project.name
  )
}
