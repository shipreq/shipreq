package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import shipreq.base.util.FxModule._
import shipreq.webapp.client.public.PublicSpaProtocols
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.lib.SnippetHelpers
import shipreq.webapp.server.protocol._

object PublicSpa extends SnippetHelpers {

  val EntryPoint = ClientSideProcInvoker(PublicSpaProtocols.EntryPoint)

  def render = {

    val fx: Fx[NodeSeq => NodeSeq] =
      for {
        initData   <- Global.logic.publicSpa.initData
        optionHtml <- Global.ssr.public(requestUrl(), initData)
      } yield {
        val initJs = "#init-js" #> EntryPoint.invokeOnLoadHtml(initData)
        optionHtml match {
          case Some(html) => initJs & ("#root *" #> html.toXml)
          case None       => initJs
        }
      }

    fx.unsafeRun().andThen(removeSnippetTag)
  }
}
