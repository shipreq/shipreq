package shipreq.webapp.client.public

import japgolly.scalajs.react.extra.router.{BaseUrl, Router}
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.protocol.{ClientProtocol, ClientSideProcImpl}
import shipreq.webapp.client.public.spa._
import PublicSpaProtocols._

@JSExportTopLevel(EntryPointName)
object Main extends ClientSideProcImpl(EntryPoint) {

  override def run(i: InitData): Unit = {
    val cp      = ClientProtocol.Default
    val spa     = new PublicSpa(i, cp)
    val baseUrl = BaseUrl.fromWindowOrigin
    val router  = Router(baseUrl, Routes.routerConfig(spa))
    Styles.addToDocument()
    router().renderIntoDOM(`#root`)
  }
}
