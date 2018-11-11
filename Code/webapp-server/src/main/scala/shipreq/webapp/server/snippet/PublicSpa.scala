package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import shipreq.base.util.FxModule._
import shipreq.webapp.client.public.PublicSpaProtocols
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.lib.SnippetHelpers
import shipreq.webapp.server.protocol._
import shipreq.webapp.ssr.SsrJvm

object PublicSpa extends SnippetHelpers {

  val EntryPoint = ClientSideProcInvoker(PublicSpaProtocols.EntryPoint)

  def render = {
    val initData = Global.logic.publicSpa.initData.unsafeRun()

    val x = SsrJvm.TEMP.public(initData)
    println()
    println(x)
    println()

    "*" #> EntryPoint.invokeOnLoadHtml(initData)
  }
}
