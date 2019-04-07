package shipreq.webapp.base.protocol2

import shipreq.base.util.Url

trait Protocol[F[_]] { self =>
  type Type
  val codec: F[Type]

  final type AndValue = Protocol.AndValue[F] { type Type = self.Type }

  final def andValue(v: Type): AndValue =
    new Protocol.AndValue[F] {
      override type Type = self.Type
      override val codec = self.codec
      override val value = v
    }
}

object Protocol {

  type Of[F[_], A] = Protocol[F] { type Type = A }

  def apply[F[_], A](c: F[A]): Of[F, A] =
    new Protocol[F] {
      override type Type = A
      override val codec = c
    }

  // ===================================================================================================================

  trait AndValue[F[_]] {
    type Type
    val codec: F[Type]
    val value: Type
  }

  object AndValue {
    type Of[F[_], A] = AndValue[F] { type Type = A }
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

    // -----------------------------------------------------------------------------------------------------------------

    trait PreparedSend[F[_]] {
      val request : Protocol.AndValue[F]
      val response: Protocol[F]
    }

    object PreparedSend {
      type Of[F[_], Req, Res] = PreparedSend[F] {
        val request : Protocol.AndValue.Of[F, Req]
        val response: Protocol.Of[F, Res]
      }

      def apply[F[_], Req, Res](req: Protocol.AndValue.Of[F, Req], res: Protocol.Of[F, Res]): Of[F, Req, Res] =
        new PreparedSend[F] {
          override val request  = req
          override val response = res
        }
    }
  }

  // ===================================================================================================================

  final case class Ajax[F[_], Req, Res](url     : Url.Relative,
                                        protocol: RequestResponse.Of[F, Req, Res])

  // ===================================================================================================================

  object WebSocket {

    /** Client can send requests (ReqRes)
      * Server can send messages (Push)
      */
    trait ClientReqServerPush[F[_]] {
      type ReqId
      type Req
      type ReqRes <: Protocol.RequestResponse[F]  { type PreparedRequestType = Req }
      type Push
      val url         : Url.Relative
      val protocolReq : Protocol.Of[F, Req]
      val protocolPush: Protocol.Of[F, Push]
    }
  }
}
