package shipreq.webapp.client.public

import japgolly.scalajs.react.extra.router.{BaseUrl, Router}
import japgolly.scalajs.react.vdom.PackageBase._
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.protocol.{AjaxClient, ClientSideProcImpl}
import shipreq.webapp.client.public.spa._

@JSExportTopLevel(PublicSpaEntryPoint.Name)
object Main extends ClientSideProcImpl(PublicSpaEntryPoint.proc) {

  override def run(i: PublicSpaEntryPoint.InitData): Unit = {
    val spa     = new PublicSpa(i, AjaxClient.Binary)
    val router  = component(i, spa)
    Styles.addToDocument()
    hydrateOrRender(router, `#root`)
  }

  def component(i: PublicSpaEntryPoint.InitData, spa: PublicSpa): VdomElement = {
    val baseUrl = BaseUrl.fromWindowOrigin
    val router  = Router(baseUrl, Routes.routerConfig(spa))
    router()
  }
}
