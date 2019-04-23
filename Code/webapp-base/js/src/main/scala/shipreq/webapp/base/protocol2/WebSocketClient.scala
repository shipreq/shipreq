package shipreq.webapp.base.protocol2

import boopickle._
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import java.time.Duration
import org.scalajs.dom._
import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success, Try}
import scalaz.{-\/, \/-}
import shipreq.base.util.JsExt._
import shipreq.base.util.{Retries, Url}
import shipreq.webapp.base.protocol2.WebSocketShared._

final class WebSocketClient[
    Req,
    ReqRes <: Protocol.RequestResponse[Pickler] { type PreparedRequestType = Req },
    Push](
    createWS          : CallbackTo[WebSocket],
    openImmediately   : Boolean,
    connectionRetries : Retries,
    onReadyStateChange: ReadyState => WebSocketClient[Req, ReqRes, Push] => Callback,
    protocolCS        : Pickler[ClientToServer[Req]],
    mkProtocolSC      : (ReqId => Protocol[Pickler]) => Pickler[ServerToClient[Push]],
    recvPush          : Push => Callback) { self =>

  import WebSocketClient._

  private val requestManager: RequestManager[ReqId, Protocol.AndValue[Pickler], Protocol[Pickler]] =
    RequestManager.arrayStore

  private val protocolSC: Pickler[ServerToClient[Push]] =
    mkProtocolSC(requestManager.getState(_).orNull)

  private case class State(instance      : Option[Instance],
                           retries       : Retries,
                           scheduled     : Option[SetTimeoutHandle],
                           prevReadyState: Option[ReadyState])

  private var state = State(None, connectionRetries, None, None)

  private var queueOldestToNewest = Vector.empty[ArrayBuffer]

  private val connect: Callback =
    Callback {
      state.instance.map(_.readyState()) match {

        case None | Some(ReadyState.Closed) =>
          state.scheduled.foreach(clearTimeout)
          val i = unsafeNewInstance()
          state = state.copy(instance = i, scheduled = None)
          if (i.isDefined)
            state = state.copy(retries = connectionRetries)
          else
            unsafeScheduleReconnect()

        case Some(r) =>
          console.warn(s"Ignoring connect with WS in readyState $r")
      }
    }

  if (openImmediately)
    connect.runNow()

  private def unsafeNewInstance(): Option[Instance] = {
    // console.info("Connecting to server...")
    createWS.map(new Instance(_)).attempt.runNow().toOption
  }

  private def unsafeScheduleReconnect(): Unit =
    state.retries.pop match {
      case Some((retry, nextRetries)) =>
        val h = setTimeout(retry.toMillis) {
          // This bit here is Schedule in websocket_client.tla
          val i = unsafeNewInstance()
          state = state.copy(instance = i, scheduled = None)
          if (i.isEmpty)
            unsafeScheduleReconnect()
        }
        state = state.copy(retries = nextRetries, scheduled = Some(h))

      case None =>
        state = state.copy(scheduled = None)
    }

  private class Instance(val ws: WebSocket) {
    ws.binaryType = "arraybuffer"
    ws.onopen     = onOpen _
    ws.onclose    = onClose _
    ws.onmessage  = onMessage _
    ws.onerror    = onError _

    private var opened = false

    def readyState(): ReadyState =
      ReadyState.byJsValue(ws.readyState)

    def isOpen(): Boolean =
      ws.readyState == WebSocket.OPEN

    private def runReadyStateChange(): Unit = {
      val r = readyState()
      if (!state.prevReadyState.contains(r)) {
        state = state.copy(prevReadyState = Some(r))
        onReadyStateChange(r)(self).runNow()
      }
    }

    private def onOpen(e: Event): Unit = {
      opened = true
      state = state.copy(retries = connectionRetries) // reset retry counter
      runReadyStateChange()
      processQueue()
    }

    def processQueue(): Unit = {
      while (queueOldestToNewest.nonEmpty) {
        val h = queueOldestToNewest.head
        ws.send(h)
        queueOldestToNewest = queueOldestToNewest.tail
      }
    }

    private def onMessage(e: MessageEvent): Unit = {
      // console.log(s"[${ws.readyState}] onmessage: ", e.data.asInstanceOf[ArrayBuffer])
      val handler: Callback =
        CallbackTo(BinaryJs.decodeFromArrayBufferUnsafe(e.data)(protocolSC)).attemptTry.flatMap {
          case Success(\/-((id, res))) => requestManager.complete(id, Success(res)) // TODO what about Failure??
          case Success(-\/(push))      => recvPush(push)
          case Failure(err)            => Callback(onException(err))
        }
      handler.runNow()
    }

    private def onException(e: Throwable): Unit = {
      val desc = s"WebSocket exception occurred. ${e.getMessage}"
      console.error(desc)
      e.printStackTrace()
      ws.close(4001, desc.take(123)) // [MDN] reason must be no longer than 123 bytes of UTF-8 text (not characters)
    }

    private def onError(e: Event): Unit = {
      val message = e.asInstanceOf[js.Dynamic].message.asInstanceOf[js.UndefOr[String]]
      val desc = s"WebSocket error occurred.${message.fold("")(" " + _)}"
      //console.error(desc, e)
      ws.close(4000, desc.take(123)) // [MDN] reason must be no longer than 123 bytes of UTF-8 text (not characters)
    }

    private def onClose(e: CloseEvent): Unit = {
      runReadyStateChange()
      failQueued(if (opened) errorClosed else errorFailed)
      unsafeScheduleReconnect()
    }
  }

  private val processQueue: Callback =
    Callback {
      for (i <- state.instance)
        if (i.isOpen())
          i.processQueue()
    }

  private def failQueued(error: Throwable): Callback = {
    requestManager.completeAll(Failure(error))
      .map(_.foreach(_.printStackTrace()))
  }

  /** If a connection is open, Send an empty message to  Useful for preventing server-side timeout and keeping the connection alive */
  val keepAlive: Callback = {
    val ab = new ArrayBuffer(0)
    Callback {
      for (i <- state.instance)
        if (i.isOpen())
          i.ws.send(ab)
    }
  }

  def send(p: ReqRes)(request: p.RequestType): CallbackTo[AsyncCallback[p.ResponseType]] = {
    type R = CallbackTo[AsyncCallback[p.ResponseType]]

    def addToQueue: R = {
      val prep = p.prepareSend(request)
      requestManager.newRequest(prep.response).flatMap { case (reqId, callback) =>
        CallbackTo {
          val msgValue = (reqId, prep.request)
          val msgAB    = BinaryJs.encodeToArrayBuffer(msgValue)(protocolCS)
          queueOldestToNewest :+= msgAB
          callback.map(_.unsafeForceType[p.ResponseType].value)
        }
      }
    }

    def rejectImmediately(error: Throwable): R =
      CallbackTo.pure(AsyncCallback.throwException(error))

    CallbackTo.byName {
      state.instance.map(_.readyState()) match {
        case None
           | Some(ReadyState.Connecting) => addToQueue
        case Some(ReadyState.Open)       => addToQueue <* processQueue
        case Some(ReadyState.Closing)    => rejectImmediately(errorClosing)
        case Some(ReadyState.Closed)     => addToQueue <* connect
      }
    }
  }
}

// =====================================================================================================================

object WebSocketClient {

  def apply(urlBase           : Url.Absolute.Base,
            protocol          : Protocol.WebSocket.ClientReqServerPush[Pickler])
           (onServerPush      : protocol.Push => Callback,
            onReadyStateChange: ReadyState => WebSocketClient[protocol.Req, protocol.ReqRes, protocol.Push] => Callback,
           ): WebSocketClient[protocol.Req, protocol.ReqRes, protocol.Push] = {
    import WebSocketShared._
    implicit def picklerReq: Pickler[protocol.Req] = protocol.req.codec
    implicit def picklerPush: Pickler[protocol.Push] = protocol.push.codec

    val url      = (urlBase.forWebSocket / protocol.url).absoluteUrl
    val createWS = CallbackTo(new WebSocket(url))

    new WebSocketClient(
      createWS,
      true,
      retries,
      onReadyStateChange,
      protocolCS,
      protocolSC(_),
      onServerPush)
  }

  private[this] val retries =
    Retries.exponentially(Duration.ofMillis(1000)).takeWhile(_.getSeconds < 6) ++
      Retries.continually(Duration.ofSeconds(8))

  // ===================================================================================================================

  val errorClosing = js.JavaScriptException("Connection is closing.")
  val errorClosed  = js.JavaScriptException("Connection is closed.")
  val errorFailed  = js.JavaScriptException("Failed to connect to server.")

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
}
