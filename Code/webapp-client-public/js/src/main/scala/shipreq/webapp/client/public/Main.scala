package shipreq.webapp.client.public

import japgolly.scalajs.react.extra.router.{BaseUrl, Router}
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.protocol2.{AjaxClient, ClientSideProcImpl}
import shipreq.webapp.client.public.PublicSpaProtocols._
import shipreq.webapp.client.public.spa._

@JSExportTopLevel(EntryPointName)
object Main extends ClientSideProcImpl(EntryPoint) {

  override def run(i: InitData): Unit = {
    val spa     = new PublicSpa(i, AjaxClient.Binary)
    val baseUrl = BaseUrl.fromWindowOrigin
    val router  = Router(baseUrl, Routes.routerConfig(spa))
    Styles.addToDocument()
    router().renderIntoDOM(`#root`)
  }
}
