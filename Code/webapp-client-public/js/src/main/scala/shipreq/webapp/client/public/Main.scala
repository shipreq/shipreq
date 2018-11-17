package shipreq.webapp.client.public

import japgolly.scalajs.react.extra.router.{BaseUrl, Router}
import japgolly.scalajs.react.vdom.PackageBase._
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.protocol.{ClientProtocol, ClientSideProcImpl}
import shipreq.webapp.client.public.spa._
import PublicSpaProtocols._

@JSExportTopLevel(EntryPointName)
object Main extends ClientSideProcImpl(EntryPoint) {

  override def run(i: InitData): Unit = {
    val cp      = ClientProtocol.Default
    val router  = component(i, cp)
    Styles.addToDocument()
    hydrateOrRender(router, `#root`)
  }

  def component(i: InitData, cp: ClientProtocol): VdomElement = {
    val spa     = new PublicSpa(i, cp)
    val baseUrl = BaseUrl.fromWindowOrigin
    val router  = Router(baseUrl, Routes.routerConfig(spa))
    router()
  }
}
