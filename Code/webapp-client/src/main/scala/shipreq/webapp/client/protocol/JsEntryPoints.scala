package shipreq.webapp.client.protocol

import boopickle.UnpickleImpl
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalaz.effect.IO
import shipreq.webapp.base.protocol.{JsEntryPoint => EP}

@JSExport(EP.client)
object JsEntryPoints {

  private def entryPoint[I](ep: EP[I, Unit])(f: I => IO[Unit]): js.Function1[String, Unit] =
    (s: String) => {
      val b = ClientProtocol.Default.base64ToBinary(s)
      val i = UnpickleImpl(ep.pi).fromBytes(b)
      f(i).unsafePerformIO()
    }

  @JSExport(EP.reactExamplesN)
  final val reactExamples = entryPoint(EP.reactExamples)(hahaa.ReactExamples.main)
}
