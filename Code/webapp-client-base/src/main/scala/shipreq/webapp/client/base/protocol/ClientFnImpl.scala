package shipreq.webapp.client.base.protocol

import boopickle.UnpickleImpl
import scala.scalajs.js.annotation.JSExport
import shipreq.webapp.base.protocol.ClientFnDecl

abstract class ClientFnImpl[I](decl: ClientFnDecl[I]) {

  @JSExport(ClientFnDecl.DefaultMethodName)
  final def main(encodedInput: String): Unit =
    run(decodeInput(encodedInput))

  final def decodeInput(s: String): I =
    UnpickleImpl(decl.pickler) fromBytes ClientProtocol.Default.base64ToBinary(s)

  def run(i: I): Unit
}

