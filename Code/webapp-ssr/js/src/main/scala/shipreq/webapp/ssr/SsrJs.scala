package shipreq.webapp.ssr

import japgolly.scalagraal.Pickled
import japgolly.scalajs.react.ReactDOMServer
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.protocol.ClientProtocol
import shipreq.webapp.client.public.PublicSpaProtocols.{InitData => PublicInitData}
import shipreq.webapp.client.public.{Main => PublicMain}

object SsrJs {

  private val cp = ClientProtocol.Noop

  @JSExportTopLevel("setUrl")
  def setUrl(url: String): Unit =
    WindowLocation.parse(url) match {
      case Some(src) =>
        val tgt = js.Dynamic.literal(
          href     = src.href,
          origin   = src.origin,
          protocol = src.protocol,
          hostname = src.hostname,
          port     = src.port,
          pathname = src.pathname,
          search   = src.search,
          hash     = src.hash,
        )
        js.Dynamic.global.window.location = tgt

      case None =>
        throw new RuntimeException("Failed to parse URL:" + url)
    }

  @JSExportTopLevel("public")
  def public(i: Pickled[PublicInitData]): String = {
    val component = PublicMain.component(i.value, cp)
    ReactDOMServer.renderToString(component)
  }
}
