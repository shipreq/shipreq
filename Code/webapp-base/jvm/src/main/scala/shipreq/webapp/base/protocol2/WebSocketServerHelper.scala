package shipreq.webapp.base.protocol2

import boopickle.Pickler
import WebSocketShared.{ClientToServer, ReqId, ServerToClient}

final class WebSocketServerHelper[Req, Push](val protocolCS: Protocol.Of[Pickler, ClientToServer[Req]],
                                             val protocolSC: Protocol.Of[Pickler, ServerToClient[Push]])

object WebSocketServerHelper {

  private val responseUnpickler: ReqId => Protocol[Pickler] =
    _ => sys.error("Server doesn't unpickle responses.")

  def apply(p: Protocol.WebSocket.ClientReqServerPush[Pickler]): WebSocketServerHelper[p.Req, p.Push] = {
    import WebSocketShared._
    implicit def picklerReq: Pickler[p.Req] = p.req.codec
    implicit def picklerPush: Pickler[p.Push] = p.push.codec
    new WebSocketServerHelper[p.Req, p.Push](
      Protocol(protocolCS),
      Protocol(protocolSC(responseUnpickler)))
  }

}
