package shipreq.webapp.client.public

import japgolly.scalajs.react.extra.router.{BaseUrl, Router}
import japgolly.scalajs.react.vdom.PackageBase._
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.feature.ErrorHandlingFeature
import shipreq.webapp.base.protocol.ajax.{AjaxClient, CommonProtocolsJs}
import shipreq.webapp.base.protocol.entrypoint.ClientSideProcImpl
import shipreq.webapp.client.public.spa._

@JSExportTopLevel(PublicSpaEntryPoint.Name)
object Main extends ClientSideProcImpl(PublicSpaEntryPoint.proc) {

  override def run(i: PublicSpaEntryPoint.InitData): Unit = {
    ErrorHandlingFeature.enable()
    Styles.addToDocument()

    val spa      = new PublicSpa(i, AjaxClient.Binary)
    val reactApp = component(i, spa)
    hydrateOrRender(reactApp, `#root`)
  }

  def component(i: PublicSpaEntryPoint.InitData, spa: PublicSpa): VdomElement = {
    val baseUrl  = BaseUrl.fromWindowOrigin
    val router   = Router(baseUrl, Routes.routerConfig(spa))
    val metadata = CommonProtocolsJs.Metadata.client(i.loggedInUser)
    val reactApp = ErrorHandlingFeature(router(), metadata)
    reactApp
  }
}
