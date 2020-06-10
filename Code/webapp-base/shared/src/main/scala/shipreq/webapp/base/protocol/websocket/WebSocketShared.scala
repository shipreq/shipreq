package shipreq.webapp.base.protocol.websocket

import boopickle.DefaultBasic._
import japgolly.univeq.UnivEq
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.Util
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._

object WebSocketShared {

  final case class ReqId(value: Int)

  object ReqId {
    implicit def univEq: UnivEq[ReqId] = UnivEq.derive
    implicit val pickler: Pickler[ReqId] = intPickler.xmap(apply)(_.value)
  }

  final case class CloseCode(value: Int)

  object CloseCode {

    /** 1000 indicates a normal closure, meaning that the purpose for
      * which the connection was established has been fulfilled.
      */
    lazy val normalClosure = apply(1000)

    /** 1001 indicates that an endpoint is "going away", such as a server
      * going down or a browser having navigated away from a page.
      */
    lazy val goingAway = apply(1001)

    /** 1002 indicates that an endpoint is terminating the connection due
      * to a protocol error.
      */
    lazy val protocolError = apply(1002)

    /** 1003 indicates that an endpoint is terminating the connection
      * because it has received a type of data it cannot accept (e.g., an
      * endpoint that understands only text data MAY send this if it
      * receives a binary message).
      */
    lazy val cannotAccept = apply(1003)

    /** Reserved. The specific meaning might be defined in the future. */
    lazy val reserved = apply(1004)

    /** 1005 is a reserved value and MUST NOT be set as a status code in a
      * Close control frame by an endpoint.  It is designated for use in
      * applications expecting a status code to indicate that no status
      * code was actually present.
      */
    lazy val noStatusCode = apply(1005)

    /** 1006 is a reserved value and MUST NOT be set as a status code in a
      * Close control frame by an endpoint.  It is designated for use in
      * applications expecting a status code to indicate that the
      * connection was closed abnormally, e.g., without sending or
      * receiving a Close control frame.
      */
    lazy val closedAbnormally = apply(1006)

    /** 1007 indicates that an endpoint is terminating the connection
      * because it has received data within a message that was not
      * consistent with the type of the message (e.g., non-UTF-8
      * data within a text message).
      */
    lazy val notConsistent = apply(1007)

    /** 1008 indicates that an endpoint is terminating the connection
      * because it has received a message that violates its policy.  This
      * is a generic status code that can be returned when there is no
      * other more suitable status code (e.g., 1003 or 1009) or if there
      * is a need to hide specific details about the policy.
      */
    lazy val violatedPolicy = apply(1008)

    /** 1009 indicates that an endpoint is terminating the connection
      * because it has received a message that is too big for it to
      * process.
      */
    lazy val tooBig = apply(1009)

    /** 1010 indicates that an endpoint (client) is terminating the
      * connection because it has expected the server to negotiate one or
      * more extension, but the server didn't return them in the response
      * message of the WebSocket handshake.  The list of extensions that
      * are needed SHOULD appear in the /reason/ part of the Close frame.
      * Note that this status code is not used by the server, because it
      * can fail the WebSocket handshake instead.
      */
    lazy val noExtension = apply(1010)

    /** 1011 indicates that a server is terminating the connection because
      * it encountered an unexpected condition that prevented it from
      * fulfilling the request.
      */
    lazy val unexpectedCondition = apply(1011)

    /** 1012 indicates that the service will be restarted. */
    lazy val serviceRestart = apply(1012)

    /** 1013 indicates that the service is experiencing overload */
    lazy val tryAgainLater = apply(1013)

    /** 1015 is a reserved value and MUST NOT be set as a status code in a
     * Close control frame by an endpoint.  It is designated for use in
     * applications expecting a status code to indicate that the
     * connection was closed due to a failure to perform a TLS handshake
     * (e.g., the server certificate can't be verified).
     */
    lazy val tlsHandshakeFailure = apply(1015)

    // Custom codes below. Must all be in the 4000 - 4999 range

    /** Runtime exception occurred */
    lazy val unhandledException = apply(4000)

    /** Error sending response */
    lazy val respondException = apply(4001)

    /** Like HTTP 401: request lacks valid authentication credentials */
    val unauthorised = apply(4002)
  }

  final case class CloseReasonPhrase(value: String)

  object CloseReasonPhrase {
    def apply(reason: String): CloseReasonPhrase =
      new CloseReasonPhrase(Util.limitUtf8Bytes(reason, 123)) // [MDN] reason must be no longer than 123 UTF-8 bytes

    val empty: CloseReasonPhrase =
      new CloseReasonPhrase("")
  }

  final case class CloseReason(code: CloseCode, phrase: CloseReasonPhrase)

  object CloseReason {
    val normalClosure = CloseReason(CloseCode.normalClosure, CloseReasonPhrase.empty)
    val clientOutOfDate = CloseReason(CloseCode.cannotAccept, CloseReasonPhrase("Client is out-of-date"))
  }

  // ===================================================================================================================
  // Client to Server

  type ClientToServer[Req] = (ReqId, Req)

  def protocolCS[Req](implicit sp: SafePickler[Req]): Protocol.Of[SafePickler, ClientToServer[Req]] =
    Protocol(sp.map(Tuple2Pickler(ReqId.pickler, _)))

  // ===================================================================================================================
  // Server to Client

  type ServerToClient[Push] = Push \/ (ReqId, Protocol.AndValue[SafePickler])

  /** TAKE CARE! The Protocol.AndValue[SafePickler] part of unpickled results will be null if no unpickler is available!
    */
  def protocolSC[Push](responseUnpickler: ReqId => Option[Protocol[SafePickler]])
                      (implicit pushCodec: SafePickler[Push]): Protocol.Of[SafePickler, ServerToClient[Push]] = {

    val pickler: Pickler[ServerToClient[Push]] =
      new Pickler[ServerToClient[Push]] {

        override def pickle(obj: ServerToClient[Push])(implicit state: PickleState): Unit =
          obj match {
            case \/-((i, pv)) =>
              state.enc.writeLong(i.value.toLong << 1)
              pv.codec.embeddedWrite(pv.value)
            case -\/(push) =>
              state.enc.writeLong(1)
              pushCodec.embeddedWrite(push)
          }

        override def unpickle(implicit state: UnpickleState): ServerToClient[Push] = {
          val header = state.dec.readLong
          if (header == 1)
            -\/(pushCodec.embeddedRead)
          else {
            val reqId = ReqId((header >> 1).toInt)
            val protocol = responseUnpickler(reqId)
            val pav: Protocol.AndValue[SafePickler] =
              protocol match {
                case Some(p) =>
                  val v = p.codec.embeddedRead
                  p.andValue(v)
                case None =>
                  null
              }
            \/-((reqId, pav))
          }
        }
      }

    Protocol(pickler.asV1(0))
  }
}
