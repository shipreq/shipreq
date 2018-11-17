package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import shipreq.base.util.FxModule._
import shipreq.base.util.Url
import shipreq.webapp.client.public.PublicSpaProtocols
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.lib.SnippetHelpers
import shipreq.webapp.server.protocol._

object PublicSpa extends SnippetHelpers {

  val EntryPoint = ClientSideProcInvoker(PublicSpaProtocols.EntryPoint)

  def render = {

    val req = request_!()
    val url = Url.Absolute(
      if (req.uri == "/" && req.request.url.last == '/')
        req.request.url.dropRight(1)
      else
        req.request.url
    )

    val fx: Fx[NodeSeq => NodeSeq] =
      for {
        initData   <- Global.logic.publicSpa.initData
        optionHtml <- Global.ssr.public(url, initData)
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
