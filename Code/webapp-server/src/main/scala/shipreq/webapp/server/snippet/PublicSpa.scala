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
    val user = currentUserOption()
    val initData = PublicSpaProtocols.InitData(Global.config.server.publicRegistration, user.map(_.username))

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
