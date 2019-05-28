package shipreq.webapp.ssr

import japgolly.scalagraal.Pickled
import japgolly.scalajs.react.ReactDOMServer
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.protocol.AjaxClient
import shipreq.webapp.client.public.PublicSpaProtocols.{InitData => PublicInitData}
import shipreq.webapp.client.public.{Main => PublicMain}
import shipreq.webapp.client.project.app.root.LoadingPage
import shipreq.webapp.client.public.spa.PublicSpa

object SsrJs {

  private val ajaxNoop: AjaxClient.Binary =
    AjaxClient.noop

  @JSExportTopLevel(SsrManifest.SetUrl)
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

  @JSExportTopLevel(SsrManifest.Public)
  def public(i: Pickled[PublicInitData]): String = {
    val spa       = new PublicSpa(i.value, ajaxNoop)
    val component = PublicMain.component(i.value, spa)
    ReactDOMServer.renderToString(component)
  }

  @JSExportTopLevel(SsrManifest.ProjectSpaLoader)
  def projectSpaLoader(i: Pickled[ProjectSpaLoaderData]): String = {
    val component = LoadingPage.Props(i.value.username, i.value.projectName).render
    ReactDOMServer.renderToStaticMarkup(component)
  }
}
