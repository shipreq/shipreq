package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import shipreq.webapp.base.protocol.{CreateProjectFn, InitDataForHomeSpa}
import shipreq.webapp.server.lib.SnippetHelpers
import shipreq.webapp.server.protocol.ClientFn
import shipreq.webapp.server.protocol.ServerProtocol._

object HomeSpa extends SnippetHelpers {

  def render = {
    val user = currentUser_!()
    val projects = daoProvider.withSession(_.getProjectCatalogue(user.id))
    val data = InitDataForHomeSpa(user.username, projects,  remoteFn(CreateProjectFn)(n => ???))
    "*" #> ClientFn.HomeSpa.runOnLoadHtml(data)
  }
}
