package shipreq.webapp.base.protocol

import boopickle.UnpickleImpl
import org.scalajs.dom
import scala.scalajs.js.annotation.JSExport

abstract class ClientSideProcImpl[Input](proc: ClientSideProc[Input]) {

  @JSExport(ClientSideProc.MainMethodName)
  final def main(encodedInput: String): Unit =
    run(decodeInput(encodedInput))

  final def decodeInput(s: String): Input = {
    val bb = BinaryJs.base64ToByteBuffer(s)
    UnpickleImpl(proc.pickler) fromBytes bb
  }

  protected def `#root` = dom.document.getElementById("root")

  def run(i: Input): Unit
}

