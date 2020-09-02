package shipreq.webapp.ssr

import japgolly.scalagraal.Pickled
import japgolly.scalajs.react.ReactDOMServer
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.protocol.ajax.AjaxClient
import shipreq.webapp.client.loaders._

/** This code is compiled into JS and executed on the JVM through Graal JS.
  */
object SsrJs {
  import SsrSharedData._

  private val ajaxNoop: AjaxClient.Binary =
    AjaxClient.never

  @JSExportTopLevel(SsrJsFunctionManifest.PublicLoader)
  def publicLoader(i: Pickled[PublicInitData]): String = {
    import shipreq.webapp.client.public.spa.PublicSpa
    import shipreq.webapp.client.public.Main
    val spa       = new PublicSpa(i.value, ajaxNoop)
    val component = Main.component(i.value, spa)
    ReactDOMServer.renderToString(component)
  }

  @JSExportTopLevel(SsrJsFunctionManifest.HomeSpaLoader)
  def homeSpaLoader(pi: Pickled[HomeSpaLoaderData]): String = {
    val i = pi.value
    val component = HomeSpaLoader.Props(i.username, i.assetManifest).render
    ReactDOMServer.renderToStaticMarkup(component)
  }

  @JSExportTopLevel(SsrJsFunctionManifest.ProjectSpaLoader)
  def projectSpaLoader(pi: Pickled[ProjectSpaLoaderData]): String = {
    val i = pi.value
    val component = ProjectSpaLoader.Props(i.username, i.projectName, i.assetManifest).render
    ReactDOMServer.renderToStaticMarkup(component)
  }
}
