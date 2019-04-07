package shipreq.webapp.client.public

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router.{BaseUrl, Router}
import org.scalajs.dom.window
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import shipreq.base.util.Url
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

    import shipreq.webapp.base.protocol2._
    import SampleProtocol._

    val wsUrlBase = Url.Absolute.Base(window.location.protocol.replace("http", "ws") + "//" + window.location.host)
    val ws = WebSocketClient(wsUrlBase, WS)(push => Callback.log("WS PUSH RECV: ", push))
    val send1 = ws.send(ReqRes.IsEven)(17).flatMap(_.map(r => println("IsEven result = " + r)).toCallback)
    val send2 = ws.send(ReqRes.Xs)(3).flatMap(_.map(r => println("XS result = " + r)).toCallback)
//    js.timers.setInterval(4000)(send1.runNow())
//    js.timers.setTimeout(2000)(js.timers.setInterval(4000)(send2.runNow()))
    js.timers.setTimeout(200) {
      send1.runNow()
      send2.runNow()
    }
  }
}
