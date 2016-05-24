package shipreq.webapp.base.protocol

import boopickle.Pickler
import BinCodecRemoteFns._

/**
 * Declaration of an exposed client-side function.
 * The server can then generate HTML/JS snippets instructing the client to invoke the function.
 */
final class ClientFnDecl[I](val objectName: String, val methodName: String)(implicit PI: Pickler[I]) {
  implicit val pickler = PI
}

object ClientFnDecl {

  final val DefaultMethodName = "m"

  def apply[I](objectName: String)(implicit PI: Pickler[I]): ClientFnDecl[I] =
    new ClientFnDecl(objectName, DefaultMethodName)(PI)

  final val ProjectSpaName = "P_p"
  val ProjectSpa = ClientFnDecl[ProjectSpa](ProjectSpaName)
}