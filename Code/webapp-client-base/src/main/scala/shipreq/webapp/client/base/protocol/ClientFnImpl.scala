package shipreq.webapp.client.base.protocol

import boopickle.UnpickleImpl
import scala.scalajs.js.annotation.JSExport
import shipreq.webapp.base.protocol.ClientSideProc

abstract class ClientSideProcImpl[Input](proc: ClientSideProc[Input]) {

  @JSExport(ClientSideProc.MainMethodName)
  final def main(encodedInput: String): Unit =
    run(decodeInput(encodedInput))

  final def decodeInput(s: String): Input =
    UnpickleImpl(proc.pickler) fromBytes ClientProtocol.Default.base64ToBinary(s)

  def run(i: Input): Unit
}

