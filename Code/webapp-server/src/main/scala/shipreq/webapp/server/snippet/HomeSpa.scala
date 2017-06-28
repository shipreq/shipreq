package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import shipreq.webapp.base.protocol.HomeSpaProtocols
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.lib.SnippetHelpers
import shipreq.webapp.server.protocol._

object HomeSpa extends SnippetHelpers {

  val EntryPoint = ClientSideProcInvoker(HomeSpaProtocols.EntryPoint)

  def render = {
    val user = currentUser_!()
    val initData = Global.logic.homeSpa.initData(user).unsafePerformIO()
    "*" #> EntryPoint.invokeOnLoadHtml(initData)
  }
}
