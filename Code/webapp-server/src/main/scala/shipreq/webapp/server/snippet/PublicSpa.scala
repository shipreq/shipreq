package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import shipreq.base.util.FxModule._
import shipreq.webapp.client.public.PublicSpaEntryPoint
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.protocol.entrypoint.ClientSideProcInvoker

object PublicSpa extends SnippetHelpers {

  val EntryPoint = ClientSideProcInvoker(PublicSpaEntryPoint.proc)

  def render = {
    val user = currentUserOption()

    val initData = PublicSpaEntryPoint.InitData(
      Global.config.server.publicRegistration,
      user.map(_.username),
      assetManifest)

    val fx: Fx[NodeSeq => NodeSeq] =
      for {
        optionHtml <- Global.ssr.public(requestUrl(), initData.loggedInUser)
      } yield {
        val initJs = "#init-js" #> EntryPoint.invokeOnLoadHtml(initData)
        optionHtml match {
          case Some(html) => initJs & ("#root *" #> html.xml)
          case None       => initJs
        }
      }

    fx.unsafeRun().andThen(removeSnippetTag)
  }
}
