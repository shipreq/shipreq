package shipreq.webapp.client.protocol

import boopickle.UnpickleImpl
import japgolly.scalajs.react.Callback
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import shipreq.webapp.base.protocol.{JsEntryPoint => EP}
import shipreq.webapp.client.app

@JSExport(EP.client)
object JsEntryPoints {

  private def entryPoint[I](ep: EP[I, Unit])(f: I => Callback): js.Function1[String, Unit] =
    (s: String) => {
      val b = ClientProtocol.Default.base64ToBinary(s)
      val i = UnpickleImpl(ep.pi).fromBytes(b)
      f(i).runNow()
    }

  @JSExport(EP.projectN)
  val project = entryPoint(EP.project)(app.ProjectSpaMain.main)
}
