package shipreq.webapp.base.protocol2

import boopickle._
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import org.scalajs.dom.{CloseEvent, Event, MessageEvent, WebSocket}
import org.scalajs.dom.console
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success, Try}
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.JsExt._
import shipreq.base.util.Url
import shipreq.webapp.base.protocol2.WebSocketShared.{ClientToServer, ReqId, ServerToClient}

final class WebSocketClient[
    Req,
    ReqRes <: Protocol.RequestResponse[Pickler] { type PreparedRequestType = Req },
    Push](
    ws: WebSocket,
//    createWS: CallbackTo[WebSocket],
    protocolCS: Pickler[ClientToServer[Req]],
    mkProtocolSC: (ReqId => Protocol[Pickler]) => Pickler[ServerToClient[Push]],
    recvPush    : Push => Callback) {

//  private var wsInstance = Option.empty[WebSocket]

  private val requestManager: WebSocketClient.RequestManager[ReqId, Protocol.AndValue[Pickler], Protocol[Pickler]] =
    WebSocketClient.RequestManager.arrayStore

  private val protocolSC: Pickler[ServerToClient[Push]] =
    mkProtocolSC(requestManager.getState(_).orNull)

//  private def reconnect(): Unit =
//    createWS.attempt.async.toCallback.runNow()

  ws.binaryType = "arraybuffer"
  ws.onopen = onopen _
  ws.onclose = onclose _
  ws.onmessage = onmessage _
  ws.onerror = onerror _

  /* 0	CONNECTING	Socket has been created. The connection is not yet open.
     1	OPEN	The connection is open and ready to communicate.
     2	CLOSING	The connection is in the process of closing.
     3	CLOSED	The connection is closed or couldn't be opened. */

  def onopen(e: Event): Unit = {
    console.log(s"[${ws.readyState}] onopen: ", e)
  }

  /** The error handler is executed when a connection with a websocket
    * has been closed with prejudice (some data couldn't be sent for example).
    */
  def onerror(e: Event): Unit = {
    // Display error message
    val msg: String =
      e.asInstanceOf[js.Dynamic]
        .message.asInstanceOf[js.UndefOr[String]]
        .fold(s"Error occurred!")("Error occurred: " + _)
    console.log(s"[${ws.readyState}] onerror: ", e)
  }

  /** The WebSocket.onclose property is an EventHandler that is called when the WebSocket
    * connection's readyState changes to CLOSED. It is called with a CloseEvent.
    */
  def onclose(e: CloseEvent): Unit = {
    console.log(s"[${ws.readyState}] onclose: ", e)
  }

  /** Useful for preventing server-side timeout and keeping the connection alive */
  val sendNothing: Callback = {
    val ab = new ArrayBuffer(0)
    Callback {
      ws.send(ab)
    }
  }

  def send(p: ReqRes)(request: p.RequestType): CallbackTo[AsyncCallback[p.ResponseType]] = {
    val prep = p.prepareSend(request)
    requestManager.newRequest(prep.response).flatMap { case (reqId, callback) =>
      // TODO unregister on err below
      CallbackTo {
        val msgValue  = (reqId, prep.request)
        val msgAB     = BinaryJs.encodeToArrayBuffer(msgValue)(protocolCS)
        ws.send(msgAB)
        callback.map(_.unsafeForceType[p.ResponseType].value)
      }
    }
  }

  def onmessage(e: MessageEvent): Unit = {
//    console.log(s"[${ws.readyState}] onmessage: ", e.data.asInstanceOf[ArrayBuffer])
    val handler: Callback =
      CallbackTo(BinaryJs.decodeFromArrayBufferUnsafe(e.data)(protocolSC)).flatMap {
        case \/-((id, res)) => requestManager.complete(id, Success(res)) // TODO what about Failure??
        case -\/(push)      => recvPush(push)
      }
    // TODO handle errors how?
    handler.runNow()
  }
}

// =====================================================================================================================

object WebSocketClient {

  def apply(urlBase: Url.Absolute.Base,
            protocol: Protocol.WebSocket.ClientReqServerPush[Pickler])
           (recvPush: protocol.Push => Callback): WebSocketClient[protocol.Req, protocol.ReqRes, protocol.Push] = {
    import WebSocketShared._
    implicit def picklerReq: Pickler[protocol.Req] = protocol.req.codec
    implicit def picklerPush: Pickler[protocol.Push] = protocol.push.codec
    val url = (urlBase / protocol.url).absoluteUrl
    val ws = new WebSocket(url)
    new WebSocketClient(ws, protocolCS, protocolSC(_), recvPush)
  }

  // ===================================================================================================================

  private[WebSocketClient] trait RequestManager[Id, A, S] {
    def newRequest(state: S): CallbackTo[(Id, AsyncCallback[A])]
    def getState(id: Id): Option[S]
    def complete(id: Id, a: Try[A]): Callback
    def completeAll(t: Try[A]): CallbackTo[List[Throwable]]
  }

  private[WebSocketClient] object RequestManager {
    private final class ArrayStore[A, S] extends RequestManager[ReqId, A, S] {
      private var prevId = 0
      private var size = 0
      private var state = new js.Array[(S, Try[A] => Callback)]

      private def get(id: ReqId): Option[(S, Try[A] => Callback)] = {
        val e = state(id.value)
        if (js.isUndefined(e))
          None
        else
          Option(e)
      }

      override def newRequest(s: S): CallbackTo[(ReqId, AsyncCallback[A])] =
        CallbackTo {
          prevId += 1
          val id = prevId
          val (ac, tryToCb) = AsyncCallback.promise[A].runNow()
          state(id) = (s, tryToCb)
          size += 1
          (ReqId(id), ac)
        }

      override def getState(id: ReqId): Option[S] =
        get(id).map(_._1)

      override def complete(reqId: ReqId, a: Try[A]): Callback =
        Callback {
          get(reqId) match {
            case Some((_, f)) =>
              assert(size > 0)
              size -= 1
              if (size == 0)
                state = new js.Array
              else
                js.special.delete(state, reqId.value)
              f(a).runNow()
            case None =>
              ()
          }
        }

      override def completeAll(t: Try[A]): CallbackTo[List[Throwable]] =
        CallbackTo {
          // Extract handlers
          var l = List.empty[Try[A] => Callback]
          state.forEachJs(l ::= _._2)

          // Clear state
          state = new js.Array
          size = 0

          // Call handlers
          l.flatMap(_(t).attempt.runNow().left.toSeq)
        }
    }

    def arrayStore[A, S]: RequestManager[ReqId, A, S] =
      new ArrayStore[A, S]
  }

  // ===================================================================================================================

  /*
  def start: Callback = {
  socket.binaryType = 'arraybuffer';


    // This will establish the connection and return the WebSocket
    def connect = CallbackTo[WebSocket] {

      // Get direct access so WebSockets API can modify state directly
      // (for access outside of a normal DOM/React callback).
      // This means that calls like .setState will now return Unit instead of Callback.
      val direct = $.withEffectsImpure

      // These are message-receiving events from the WebSocket "thread".

      def onopen(e: Event): Unit = {
        // Indicate the connection is open
        direct.modState(_.log("Connected."))
      }

      def onmessage(e: MessageEvent): Unit = {
        // Echo message received
        direct.modState(_.log(s"Echo: ${e.data.toString}"))
      }

      def onerror(e: Event): Unit = {
        // Display error message
        val msg: String =
          e.asInstanceOf[js.Dynamic]
            .message.asInstanceOf[js.UndefOr[String]]
            .fold(s"Error occurred!")("Error occurred: " + _)
        direct.modState(_.log(msg))
      }

      def onclose(e: CloseEvent): Unit = {
        // Close the connection
        direct.modState(_.copy(ws = None).log(s"""Closed. Reason = "${e.reason}""""))
      }

      // Create WebSocket and setup listeners
      val ws = new WebSocket(url)
      ws.onopen = onopen _
      ws.onclose = onclose _
      ws.onmessage = onmessage _
      ws.onerror = onerror _
      ws
    }

    // Here use attempt to catch any exceptions in connect
    connect.attempt.flatMap {
      case Right(ws)   => $.modState(_.log(s"Connecting to $url ...").copy(ws = Some(ws)))
      case Left(error) => $.modState(_.log(s"Error connecting: ${error.getMessage}"))
    }
   */
}
