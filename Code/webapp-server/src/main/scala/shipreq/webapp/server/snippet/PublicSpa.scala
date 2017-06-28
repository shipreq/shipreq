package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import shipreq.webapp.base.protocol.PublicSpaProtocols
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.lib.SnippetHelpers
import shipreq.webapp.server.protocol._

object PublicSpa extends SnippetHelpers {

  val EntryPoint = ClientSideProcInvoker(PublicSpaProtocols.EntryPoint)

  def render = {
    val initData = Global.logic.publicSpa.initData.unsafePerformIO()
    "*" #> EntryPoint.invokeOnLoadHtml(initData)
  }
}
