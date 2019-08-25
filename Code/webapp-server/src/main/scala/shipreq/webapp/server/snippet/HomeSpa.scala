package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import shipreq.base.util.FxModule._
import shipreq.webapp.base.protocol.HomeSpaEntryPoint
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.lib.SnippetHelpers
import shipreq.webapp.server.protocol._
import shipreq.webapp.ssr.SsrSharedData.HomeSpaLoaderData

object HomeSpa extends SnippetHelpers {

  val EntryPoint = ClientSideProcInvoker(HomeSpaEntryPoint.proc)

  def render = {
    val user       = currentUser_!()
    val initData   = Global.logic.homeSpa.initData(user).unsafeRun()
    val loaderData = HomeSpaLoaderData(user.username)
    val loaderHtml = Global.ssr.homeSpaLoader(loaderData).unsafeRun().fold(NodeSeq.Empty)(_.xml)

    "*" #> (loaderHtml :+ EntryPoint.invokeOnLoadHtml(initData))
  }
}
