package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import shipreq.webapp.client.public.PublicSpaProtocols
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.lib.SnippetHelpers
import shipreq.webapp.server.protocol._

object PublicSpa extends SnippetHelpers {

  val EntryPoint = ClientSideProcInvoker(PublicSpaProtocols.EntryPoint)

  def render = {
    val user = currentUserOption()
    val initData = PublicSpaProtocols.InitData(Global.config.publicRegistration, user.map(_.username))
    "*" #> EntryPoint.invokeOnLoadHtml(initData)
  }
}
