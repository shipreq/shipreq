package shipreq.webapp.base.protocol.websocket

import WebSocketShared.{ClientToServer, ReqId, ServerToClient}
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.binary.SafePickler

final class WebSocketServerHelper[Req, Push](val protocolCS: Protocol.Of[SafePickler, ClientToServer[Req]],
                                             val protocolSC: Protocol.Of[SafePickler, ServerToClient[Push]])

object WebSocketServerHelper {

  private val responseUnpickler: ReqId => Option[Protocol[SafePickler]] =
    _ => ErrorMsg("Server doesn't unpickle responses.").throwException()

  def apply(p: Protocol.WebSocket.ClientReqServerPush[SafePickler]): WebSocketServerHelper[p.Req, p.Push] = {
    import WebSocketShared._
    implicit def picklerReq: SafePickler[p.Req] = p.req.codec
    implicit def picklerPush: SafePickler[p.Push] = p.push.codec
    new WebSocketServerHelper[p.Req, p.Push](
      protocolCS,
      protocolSC(responseUnpickler))
  }

}
