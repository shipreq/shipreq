package shipreq.webapp.base.protocol

import boopickle._
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import org.scalajs.dom.{CloseEvent, Event, MessageEvent, console}
import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success, Try}
import scalaz.{-\/, \/-}
import shipreq.base.util.JsExt._
import shipreq.base.util.{BinaryData, ErrorMsg, Retries, Url}
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.protocol.WebSocket.ReadyState
import shipreq.webapp.base.protocol.WebSocketShared._

trait WebSocketClient[ReqRes <: Protocol.RequestResponse[Pickler]] {
  val readyState: CallbackTo[Option[ReadyState]]
  val keepAlive: Callback
  def send(p: ReqRes)(request: p.RequestType): CallbackTo[AsyncCallback[p.ResponseType]]
  def invoker(p: ReqRes): ServerSideProcInvoker[p.RequestType, ErrorMsg, p.ResponseType]
  val connect: Callback
  val close: Callback
}

// =====================================================================================================================

object WebSocketClient {

  trait WithoutCallbacks[ReqRes <: Protocol.RequestResponse[Pickler], Push] {
    def build(onServerPush      : Push => Callback,
              onReadyStateChange: WebSocketClient[ReqRes] => ReadyState => Callback,
              logger            : LoggerJs.Dsl): WebSocketClient[ReqRes]
  }

  def apply(u: Url.Absolute.Base,
            p: Protocol.WebSocket.ClientReqServerPush[Pickler],
            r: Retries): WithoutCallbacks[p.ReqRes, p.Push] = {
    val url      = (u.forWebSocket / p.url).absoluteUrl
    val createWS = CallbackTo(WebSocket(url))
    apply(createWS, p, r)
  }

  def apply(w: CallbackTo[WebSocket],
            p: Protocol.WebSocket.ClientReqServerPush[Pickler],
            r: Retries): WithoutCallbacks[p.ReqRes, p.Push] =
    new WithoutCallbacks[p.ReqRes, p.Push] {
      override def build(onServerPush      : p.Push => Callback,
                         onReadyStateChange: WebSocketClient[p.ReqRes] => ReadyState => Callback,
                         logger            : LoggerJs.Dsl) =
        new Impl(
          w,
          r,
          onReadyStateChange,
          protocolCS(p.req.codec),
          protocolSC(_)(p.push.codec),
          onServerPush,
          logger)
    }

  // ===================================================================================================================

  final class Impl[
      Req,
      ReqRes <: Protocol.RequestResponse[Pickler] { type PreparedRequestType = Req },
      Push](
      createWS          : CallbackTo[WebSocket],
      connectionRetries : Retries,
      onReadyStateChange: WebSocketClient[ReqRes] => ReadyState => Callback,
      protocolCS        : Protocol.Of[Pickler, ClientToServer[Req]],
      mkProtocolSC      : (ReqId => Protocol[Pickler]) => Protocol.Of[Pickler, ServerToClient[Push]],
      recvPush          : Push => Callback,
      logger            : LoggerJs.Dsl) extends WebSocketClient[ReqRes] { self =>

    private val requestManager: RequestManager[ReqId, Protocol.AndValue[Pickler], Protocol[Pickler]] =
      RequestManager.arrayStore

    private val protocolSC: Protocol.Of[Pickler, ServerToClient[Push]] =
      mkProtocolSC(requestManager.getState(_).orNull)

    private case class State(instance      : Option[Instance],
                             retries       : Retries,
                             scheduled     : Option[SetTimeoutHandle],
                             prevReadyState: Option[ReadyState])

    private var state = State(None, connectionRetries, None, None)

    private var queueOldestToNewest = Vector.empty[BinaryData]

    override val connect: Callback =
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

    private def unsafeNewInstance(): Option[Instance] = {
      // console.info("Connecting to server...")
      createWS.map(new Instance(_)).attempt.runNow() match {
        case Right(i) => Some(i)
        case Left(e) =>
          logger.runNow(_.warn(s"Failed to create WebSocket instance.") << Callback(e.printStackTrace()))
          None
      }
    }

    private def unsafeScheduleReconnect(): Unit =
      state.retries.pop match {
        case Some((retry, nextRetries)) =>
          logger.runNow(_.info(s"WebSocketClient: retry connection in ${retry.toMillis} ms..."))
          val h = setTimeout(retry.toMillis) {
            // This bit here is Schedule in websocket_client.tla
            val i = unsafeNewInstance()
            state = state.copy(instance = i, scheduled = None)
            if (i.isEmpty)
              unsafeScheduleReconnect()
          }
          state = state.copy(retries = nextRetries, scheduled = Some(h))

        case None =>
          logger.runNow(_.info("WebSocketClient: out of retries. Leaving disconnected."))
          state = state.copy(scheduled = None)
          unsafeFailQueued(errorClosed)
      }

    private val onReadyStateChange2 = onReadyStateChange(this)

    private class Instance(val ws: WebSocket) {
      ws.binaryType.set(WebSocket.BinaryType.ArrayBuffer)
      ws.onOpen    .set(onOpen _)
      ws.onClose   .set(onClose _)
      ws.onMessage .set(onMessage _)
      ws.onError   .set(onError _)

      private var opened = false

      ws.readyState() match {
        case ReadyState.Open =>
          onOpened()

        case ReadyState.Closed =>
          // Sometimes (like when the security policy blocks the connection), the WebSocket starts in a closed state
          // without sending an onClose event. Catch that here so that the normal retry process kicks in.
          onClosed()

          case ReadyState.Closing
             | ReadyState.Connecting => ()
        }

      def readyState(): ReadyState =
        ws.readyState()

      def isOpen(): Boolean =
        ws.readyState() == ReadyState.Open

      private def runReadyStateChange(): Unit = {
        val r = readyState()
        if (!state.prevReadyState.contains(r)) {
          state = state.copy(prevReadyState = Some(r))
          onReadyStateChange2(r).runNow()
        }
      }

      private def onOpen(e: Event): Unit =
        onOpened()

      private def onOpened(): Unit = {
        opened = true
        state = state.copy(retries = connectionRetries) // reset retry counter
        runReadyStateChange()
        processQueue()
      }

      def processQueue(): Unit = {
        while (queueOldestToNewest.nonEmpty) {
          val h = queueOldestToNewest.head
          try
            ws.send(h.unsafeArrayBuffer)
          catch {
            case t: Throwable =>
              logger.runNow(_.warn(s"WebSocket.send($h) failed") << Callback(t.printStackTrace()))
              throw t
          }
          queueOldestToNewest = queueOldestToNewest.tail
        }
      }

      private def onMessage(e: MessageEvent): Unit = {
        // console.log(s"[${ws.readyState}] onmessage: ", e.data.asInstanceOf[ArrayBuffer])
        def msg = e.data.asInstanceOf[ArrayBuffer]

        val decode = CallbackTo(BinaryJs.decodeUnsafeFromArrayBuffer(msg, protocolSC))

        val handler: Callback =
          decode.attemptTry.flatMap {
            case Success(\/-((id, res))) =>
              logger(_.debug(s"WebSocketClient received response to req #${id.value}: ${res.value}")) >>
                requestManager.complete(id, Success(res))

            case Success(-\/(push)) =>
              logger(_.debug(s"WebSocketClient received push: $push")) >>
                recvPush(push)

            case Failure(err) =>
              logger(_.error(s"WebSocketClient failed to process msg: ${BinaryData.fromArrayBuffer(msg)}\n$err")).attempt >>
                Callback(onException(err))
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
        onClosed()
      }

      private def onClosed(): Unit = {
        runReadyStateChange()
        unsafeFailQueued(if (opened) errorClosed else errorFailed)
        unsafeScheduleReconnect()
      }
    }

    private val processQueue: Callback =
      Callback {
        for (i <- state.instance)
          if (i.isOpen())
            i.processQueue()
      }

    private def unsafeFailQueued(error: Throwable): Unit =
      requestManager.completeAll(Failure(error))
        .map(_.foreach(_.printStackTrace()))
        .runNow()

    override val close: Callback =
      Callback {
        state = state.copy(retries = Retries.none)
        for (i <- state.instance)
          if (i.isOpen())
            i.ws.close()
      }

    override val readyState: CallbackTo[Option[ReadyState]] =
      CallbackTo(state.instance.map(_.readyState()))

    /** If a connection is open, Send an empty message to  Useful for preventing server-side timeout and keeping the connection alive */
    override val keepAlive: Callback = {
      val ab = new ArrayBuffer(0)
      Callback {
        for (i <- state.instance)
          if (i.isOpen())
            i.ws.send(ab)
      }
    }

    override def send(p: ReqRes)(request: p.RequestType): CallbackTo[AsyncCallback[p.ResponseType]] = {
      type R = CallbackTo[AsyncCallback[p.ResponseType]]

      def addToQueue: R = {
        val prep = p.prepareSend(request)
        requestManager.newRequest(prep.response).flatMap { case (reqId, callback) =>
          CallbackTo {
            val msgValue = (reqId, prep.request)
            val msgBin    = BinaryJs.encode(protocolCS)(msgValue)
            queueOldestToNewest :+= msgBin
            callback.map(_.unsafeForceType[p.ResponseType].value)
          }
        }
      }

      def rejectImmediately(error: Throwable): R =
        CallbackTo.pure(AsyncCallback.throwException(error))

      CallbackTo.byName {
        state.instance.map(_.readyState()) match {
          case Some(ReadyState.Connecting) => addToQueue
          case Some(ReadyState.Open)       => addToQueue <* processQueue
          case Some(ReadyState.Closing)    => rejectImmediately(errorClosing)
          case None
             | Some(ReadyState.Closed)     => addToQueue <* connect
        }
      }
    }

    override def invoker(p: ReqRes): ServerSideProcInvoker[p.RequestType, ErrorMsg, p.ResponseType] =
      ServerSideProcInvoker.viaAsyncCallback(send(p)(_))
  }

  private val errorClosing = js.JavaScriptException("Connection is closing.")
  private val errorClosed  = js.JavaScriptException("Connection is closed.")
  private val errorFailed  = js.JavaScriptException("Failed to connect to server.")

  // ===================================================================================================================

  private trait RequestManager[Id, A, S] {
    def newRequest(state: S): CallbackTo[(Id, AsyncCallback[A])]
    def getState(id: Id): Option[S]
    def complete(id: Id, a: Try[A]): Callback
    def completeAll(t: Try[A]): CallbackTo[List[Throwable]]
  }

  private object RequestManager {
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
              assert(size > 0, s"WebSocketClient.RequestManager: size=$size, reqId=${reqId.value}, state=$state")
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
