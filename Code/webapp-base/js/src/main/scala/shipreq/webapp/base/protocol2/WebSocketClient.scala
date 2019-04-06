package shipreq.webapp.base.protocol2

import boopickle._
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import java.nio.ByteBuffer
import org.scalajs.dom.{CloseEvent, Event, MessageEvent, WebSocket}
import org.scalajs.dom.console
import scala.scalajs.js
import scala.util.{Failure, Success, Try}
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.JsExt._

final class WebSocketClient[
    ReqRes <: Protocol.RequestResponse[Pickler],
    Push](
    ws: WebSocket,
//    createWS: CallbackTo[WebSocket],
    protocolCS: Pickler[(Int, ByteBuffer)], // ByteBuffer is actually ReqRes#PreparedRequestType
    protocolSC: Pickler[Push \/ (Int, ByteBuffer)],
    recvPush    : Push => Callback) {

//  private var wsInstance = Option.empty[WebSocket]

  private val requestManager: WebSocketClient.RequestManager[Int, ByteBuffer] =
    WebSocketClient.RequestManager.arrayStore

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

  def send(p: ReqRes)(request: p.RequestType): CallbackTo[AsyncCallback[p.ResponseType]] =
    requestManager.newRequest.flatMap { case (reqId, callback) =>
      // TODO unregister on err below
      CallbackTo {
        val prep      = p.prepareSend(request)
        val reqBB     = BinaryJs.encodetoByteBufferP(prep.request)
        val msgValue  = (reqId, reqBB)
        val msgBinary = BinaryJs.encode(msgValue)(protocolCS)
        ws.send(msgBinary.buffer)
        callback.map(UnpickleImpl(prep.response.codec).fromBytes(_))
      }
    }

  def onmessage(e: MessageEvent): Unit = {
    console.log(s"[${ws.readyState}] onmessage: ", e)
    val handler: Callback =
      CallbackTo(BinaryJs.decodeUnsafe(e.data)(protocolSC)).flatMap {
        case \/-((id, res)) => requestManager.complete(id, Success(res)) // TODO what about Failure??
        case -\/(push)      => recvPush(push)
      }
    // TODO handle errors how?
    handler.runNow()
  }

}


object WebSocketClient {


  trait RequestManager[Id, A] {
    val newRequest: CallbackTo[(Id, AsyncCallback[A])]
    def complete(id: Id, a: Try[A]): Callback
    def completeAll(t: Try[A]): CallbackTo[List[Throwable]]
  }

  object RequestManager {
    private final class ArrayStore[A] extends RequestManager[Int, A] {
      private var prevId = 0
      private var size = 0
      private var state = new js.Array[Try[A] => Callback]

      override val newRequest: CallbackTo[(Int, AsyncCallback[A])] =
        CallbackTo {
          prevId += 1
          val id = prevId
          val (ac, tryToCb) = AsyncCallback.promise[A].runNow()
          state(id) = tryToCb
          size += 1
          (id, ac)
        }

      override def complete(id: Int, a: Try[A]): Callback =
        Callback {
          val f = state(id)
          if (!js.isUndefined(f)) {
            assert(size > 0)
            size -= 1
            if (size == 0)
              state = new js.Array
            else
              js.special.delete(state, id)
            f(a).runNow()
          }
        }

      override def completeAll(t: Try[A]): CallbackTo[List[Throwable]] =
        CallbackTo {
          // Extract handlers
          var l = List.empty[Try[A] => Callback]
          state.forEachJs(l ::= _)

          // Clear state
          state = new js.Array
          size = 0

          // Call handlers
          l.flatMap(_(t).attempt.runNow().left.toSeq)
        }
    }

    def arrayStore[A]: RequestManager[Int, A] =
      new ArrayStore[A]
  }

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

//sealed trait WebSocketError
//object WebSocketError {
//  case object Xxxx extends WebSocketError
//  final case class Xxxx() extends WebSocketError
//}