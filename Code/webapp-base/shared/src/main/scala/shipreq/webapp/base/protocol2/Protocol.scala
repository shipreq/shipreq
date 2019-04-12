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

    def unsafeForceType[A]: AndValue.Of[F, A] =
      this.asInstanceOf[AndValue.Of[F, A]]
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

    type Simple[F[_], Req, Res] = RequestResponse[F] {
      type RequestType         = Req
      type ResponseType        = Res
      type PreparedRequestType = Req
    }

    def simple[F[_], Req, Res](res: Protocol.Of[F, Res]): Simple[F, Req, Res] =
      new RequestResponse[F] {
        override type RequestType         = Req
        override type ResponseType        = Res
        override type PreparedRequestType = Req
        override def prepareSend(r: Req) = PreparedSend(r, res)
      }

    // -----------------------------------------------------------------------------------------------------------------
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
    val url     : Url.Relative
    val protocol: RequestResponse[F]
    val prepReq : Protocol.Of[F, protocol.PreparedRequestType]
    final def prepareSend(r: protocol.RequestType) = protocol.prepareSend(r)
  }

  object Ajax {

    final case class Simple[F[_], _Req, _Res](url: Url.Relative,
                                              req: Protocol.Of[F, _Req],
                                              res: Protocol.Of[F, _Res]) extends Ajax[F] {
      type Req = _Req
      type Res = _Res
      override val protocol = RequestResponse.simple[F, Req, Res](res)
      override val prepReq  = req
    }

  }

  // ===================================================================================================================

  object WebSocket {

    /** Client can send requests (ReqRes)
      * Server can send messages (Push)
      */
    trait ClientReqServerPush[F[_]] {
      type ReqId
      type ReqRes <: Protocol.RequestResponse[F] { type PreparedRequestType = Req }
      final type Req = req.Type
      final type Push = push.Type
      val url: Url.Relative
      val req: Protocol[F]
      val push: Protocol[F]
    }
  }
}
