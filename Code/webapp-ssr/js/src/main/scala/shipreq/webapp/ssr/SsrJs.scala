package shipreq.webapp.ssr

import japgolly.scalagraal.Pickled
import japgolly.scalajs.react.ReactDOMServer
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.protocol.AjaxClient
import shipreq.webapp.client.loaders._

/** This code is compiled into JS and executed on the JVM through Graal JS.
  */
object SsrJs {
  import SsrSharedData._

  private val ajaxNoop: AjaxClient.Binary =
    AjaxClient.noop

  @JSExportTopLevel(SsrJsFunctionManifest.Public)
  def public(i: Pickled[PublicInitData]): String = {
    import shipreq.webapp.client.public.spa.PublicSpa
    import shipreq.webapp.client.public.Main
    val spa       = new PublicSpa(i.value, ajaxNoop)
    val component = Main.component(i.value, spa)
    ReactDOMServer.renderToString(component)
  }

  @JSExportTopLevel(SsrJsFunctionManifest.HomeSpaLoader)
  def homeSpaLoader(i: Pickled[HomeSpaLoaderData]): String = {
    val component = HomeSpaLoader.Props(i.value.username).render
    ReactDOMServer.renderToStaticMarkup(component)
  }

  @JSExportTopLevel(SsrJsFunctionManifest.ProjectSpaLoader)
  def projectSpaLoader(i: Pickled[ProjectSpaLoaderData]): String = {
    val component = ProjectSpaLoader.Props(i.value.username, i.value.projectName).render
    ReactDOMServer.renderToStaticMarkup(component)
  }
}
