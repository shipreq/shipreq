package shipreq.webapp.base.protocol2

import shipreq.base.util.Url

trait Protocol[F[_]] { self =>
  type Type
  val codec: F[Type]
}

object Protocol {

  type Of[F[_], A] = Protocol[F] { type Type = A }

  def apply[F[_], A](c: F[A]): Of[F, A] =
    new Protocol[F] {
      override type Type = A
      override val codec = c
    }

  // ===================================================================================================================

  trait RequestResponse[F[_]] {
    type RequestType
    type ResponseType

    final type PreparedSend = RequestResponse.PreparedSend.Of[F, PreparedRequestType, ResponseType]
    type PreparedRequestType
    def prepareSend(r: RequestType): PreparedSend
  }

  object RequestResponse {

    type Of[F[_], Req, Res] = RequestResponse[F] {
      type RequestType  = Req
      type ResponseType = Res
    }

    trait PreparedSend[F[_], Req] {
      val request : Req
      val response: Protocol[F]
    }

    object PreparedSend {
      type Of[F[_], Req, Res] = PreparedSend[F, Req] {
        val request : Req
        val response: Protocol.Of[F, Res]
      }

      def apply[F[_], Req, Res](req: Req, res: Protocol.Of[F, Res]): Of[F, Req, Res] =
        new PreparedSend[F, Req] {
          override val request  = req
          override val response = res
        }
    }
  }

  // ===================================================================================================================

  trait Ajax[F[_]] {
    val url            : Url.Relative
    val protocol       : RequestResponse[F]
    val protocolPrepReq: Protocol.Of[F, protocol.PreparedRequestType]
  }

  // ===================================================================================================================

  object WebSocket {

    /** Client can send requests (ReqRes)
      * Server can send messages (Push)
      */
    trait ClientReqServerPush[F[_]] {
      type ReqId
      type Req
      type ReqRes <: Protocol.RequestResponse[F] { type PreparedRequestType = Req }
      type Push
      val url         : Url.Relative
      val protocolReq : Protocol.Of[F, Req]
      val protocolPush: Protocol.Of[F, Push]
    }
  }
}
