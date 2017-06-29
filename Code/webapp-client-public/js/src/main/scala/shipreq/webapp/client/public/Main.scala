package shipreq.webapp.client.public

import japgolly.scalajs.react.extra.router.{BaseUrl, Router}
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.webapp.base.protocol.PublicSpaProtocols
import shipreq.webapp.base.protocol.{ClientProtocol, ClientSideProcImpl}
import shipreq.webapp.client.public.root._

@JSExportTopLevel(PublicSpaProtocols.EntryPointName)
object Main extends ClientSideProcImpl(PublicSpaProtocols.EntryPoint) {

  override def run(i: Unit): Unit = {
    val cp      = ClientProtocol.Default
    val root    = new Root(cp)
    val baseUrl = BaseUrl.fromWindowOrigin
    val router  = Router(baseUrl, Routes.routerConfig(root))
    Styles.addToDocument()
    router().renderIntoDOM(`#root`)
  }
}
