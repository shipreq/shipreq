package shipreq.webapp.base.protocol

import boopickle.UnpickleImpl
import japgolly.scalajs.react.ReactDOM
import japgolly.scalajs.react.vdom.VdomElement
import org.scalajs.dom
import scala.scalajs.js.annotation.JSExport

abstract class ClientSideProcImpl[Input](proc: ClientSideProc[Input]) {

  @JSExport(ClientSideProc.MainMethodName)
  final def main(encodedInput: String): Unit =
    run(decodeInput(encodedInput))

  final def decodeInput(s: String): Input =
    UnpickleImpl(proc.pickler) fromBytes ClientProtocol.Default.base64ToBinary(s)

  protected def `#root` = dom.document.getElementById("root")

  protected def hydrateOrRender(element  : VdomElement,
                                container: dom.Element): Unit = {
    if (container.hasChildNodes()) {
      ReactDOM.raw.hydrate(element.rawElement, container)
    } else {
      element.renderIntoDOM(container)
    }
  }

  def run(i: Input): Unit
}
