package shipreq.webapp.base.protocol.websocket

import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import japgolly.univeq._
import org.scalajs.dom.{CloseEvent, Event, MessageEvent, console, window}
import scala.annotation.elidable
import scala.scalajs.js
import scala.scalajs.js.timers.SetTimeoutHandle
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success, Try}
import scalaz.{-\/, \/-}
import shipreq.base.util.JsExt._
import shipreq.base.util._
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.websocket.WebSocket.ReadyState
import shipreq.webapp.base.protocol.websocket.WebSocketShared._

trait WebSocketClient[ReqRes <: Protocol.RequestResponse[SafePickler]] {
  val readyState: CallbackTo[Option[ReadyState]]
  val keepAlive: Callback
  def send(p: ReqRes)(request: p.RequestType): CallbackTo[AsyncCallback[p.ResponseType]]
  def invoker(p: ReqRes): ServerSideProcInvoker[p.RequestType, ErrorMsg, p.ResponseType]
  def connect: Callback
  val close: Callback
}

// =====================================================================================================================

object WebSocketClient {

  trait Builder[ReqRes <: Protocol.RequestResponse[SafePickler], Push] {
    def build(reauthorise  : AsyncCallback[Permission],
              onServerPush : Push => Callback,
              onStateChange: WebSocketClient[ReqRes] => State => Callback,
              timers       : JsTimers,
              logger       : LoggerJs.Dsl): WebSocketClient[ReqRes]
  }

  object Builder {

    def apply(u: Url.Absolute.Base,
              p: Protocol.WebSocket.ClientReqServerPush[SafePickler],
              r: Retries): Builder[p.ReqRes, p.Push] = {
      val url      = (u.forWebSocket / p.url).absoluteUrl
      val createWS = CallbackTo(WebSocket(url))
      apply(createWS, p, r)
    }

    def apply(w: CallbackTo[WebSocket],
              p: Protocol.WebSocket.ClientReqServerPush[SafePickler],
              r: Retries): Builder[p.ReqRes, p.Push] =
      new Builder[p.ReqRes, p.Push] {
        override def build(reauthorise  : AsyncCallback[Permission],
                           onServerPush : p.Push => Callback,
                           onStateChange: WebSocketClient[p.ReqRes] => State => Callback,
                           timers       : JsTimers,
                           logger       : LoggerJs.Dsl) =
          new Impl(
            w,
            r,
            reauthorise,
            onStateChange,
            protocolCS(p.req.codec),
            protocolSC(_)(p.push.codec),
            onServerPush,
            timers,
            logger)
      }
  }

  object CloseReasons {
    val parseError = CloseReason(CloseCode.protocolError, CloseReasonPhrase("Failed to parse server response"))
  }

  sealed trait State
  object State {
    case object Unauthorised extends State
    final case class Authorised(readyState: ReadyState) extends State

    implicit def univEq: UnivEq[State] = UnivEq.derive
  }

  // ===================================================================================================================

  final class Impl[
      Req,
      ReqRes <: Protocol.RequestResponse[SafePickler] { type PreparedRequestType = Req },
      Push](
      createWS          : CallbackTo[WebSocket],
      connectionRetries : Retries,
      reauthorise       : AsyncCallback[Permission],
      onStateChange     : WebSocketClient[ReqRes] => State => Callback,
      protocolCS        : Protocol.Of[SafePickler, ClientToServer[Req]],
      mkProtocolSC      : (ReqId => Option[Protocol[SafePickler]]) => Protocol.Of[SafePickler, ServerToClient[Push]],
      recvPush          : Push => Callback,
      timers            : JsTimers,
      logger            : LoggerJs.Dsl) extends WebSocketClient[ReqRes] { self =>

    private val requestManager: RequestManager[ReqId, Protocol.AndValue[SafePickler], Protocol[SafePickler]] =
      RequestManager.arrayStore

    private val protocolSC: Protocol.Of[SafePickler, ServerToClient[Push]] =
      mkProtocolSC(requestManager.getState)

    /** All state for the WebSocket client.
      *
      * This is impure because of .instance.
      *
      * Unfortunately splitting this into multiple, clearer cases isn't an option. Even if you just had Connected
      * and Disconnected as cases, Connected holds a WebSocket instance which has it's own mutable state, and can change
      * to Closed without gives us a chance to change the state over to Disconnected.
      *
      * Correctness, confidence and sanity are instead achieved by formal specification in `websocket_client.tla`.
      *
      * @param authorised Whether we think we're logged in or not.
      * @param instance An optional WebSocket instance that may or may not be connected. See it's readyState.
      * @param retries Remaining retries when attempting to reconnect.
      * @param scheduled A scheduled task that will attempt to (re)connect when it eventually executes.
      * @param prevState The last [[State]] used to notify readyState-change listeners. Used to prevent sending
      *                  consecutive, identical notifications.
      */
    private case class InternalState(authorised: Boolean,
                                     instance  : Option[Instance],
                                     retries   : Retries,
                                     scheduled : Option[SetTimeoutHandle],
                                     prevState : Option[State])

    private var state = InternalState(
      authorised = true,
      instance   = None,
      retries    = connectionRetries,
      scheduled  = None,
      prevState  = None)

    private var queueOldestToNewest = Vector.empty[(ReqId, BinaryData)]

    private val readyStateCB: CallbackTo[Option[ReadyState]] =
      CallbackTo(state.instance.map(_.readyState()))

    private def setPublicState(newState: State): Callback =
      Callback {
        if (!state.prevState.exists(_ ==* newState)) {
          state = state.copy(prevState = Some(newState))
          onReadyStateChange2(newState).runNow()
        }
      }

    override lazy val connect: Callback = {
      val attemptConnect: Callback =
        Callback {
          state.scheduled.foreach(timers.clearTimeout)
          val i = unsafeNewInstance()
          state = state.copy(instance = i, scheduled = None)
          if (i.isDefined)
            state = state.copy(retries = connectionRetries)
          else
            unsafeScheduleReconnect()
        }

      val reauthoriseAndReconnect: Callback =
        reauthorise.attempt.flatMap {

          case Right(Allow) =>
            AsyncCallback.point {
              state = state.copy(authorised = true)
            } >> connect.asAsyncCallback

          case Right(Deny) | Left(_) =>
            AsyncCallback.point {
              unsafeFailQueued(errorUnauthorised)
            }

        }.toCallback

      readyStateCB.flatMap {
        case None | Some(ReadyState.Closed) =>
          if (state.authorised)
            attemptConnect
          else
            reauthoriseAndReconnect
        case Some(ReadyState.Connecting | ReadyState.Open | ReadyState.Closing) =>
          Callback.empty
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
          val h = timers.setTimeout(retry.toMillis) {
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

    private val onReadyStateChange2 = onStateChange(this)

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
          onClosed(None)

          case ReadyState.Closing
             | ReadyState.Connecting => ()
        }

      @elidable(elidable.INFO)
      override def toString = s"WebSocketClient.Instance(${readyState()})"

      def readyState(): ReadyState =
        ws.readyState()

      def isOpen(): Boolean =
        ws.readyState() == ReadyState.Open

      private def runReadyStateChange(): Unit = {
        val newState = State.Authorised(readyState())
        setPublicState(newState).runNow()
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
          val (reqId, payload) = queueOldestToNewest.head

          if (requestManager.getState(reqId).isDefined)
            try
              ws.send(payload.toArrayBuffer)
            catch {
              case t: Throwable =>
                logger.runNow(l => l.exception(t) >> l.warn(s"WebSocket.send($payload) failed"))
                throw t
            }

          queueOldestToNewest = queueOldestToNewest.tail
        }
      }

      private def onMessage(e: MessageEvent): Unit = {
        // console.log(s"[${ws.readyState}] onmessage: ", e.data.asInstanceOf[ArrayBuffer])
        def msg = e.data.asInstanceOf[ArrayBuffer]

        val decode = CallbackTo(protocolSC.codec.decode(BinaryData.unsafeFromArrayBuffer(msg)))

        val handler: Callback =
          decode.attempt.flatMap {
            case Right(\/-(\/-((id, null)))) =>
              logger(_.debug(s"Unable to decode response to req #${id.value}; request has been removed")) >>
                requestManager.remove(id)

            case Right(\/-(\/-((id, res)))) =>
              logger(_.debug(s"WebSocketClient received response to req #${id.value}: ${res.value}")) >>
                requestManager.complete(id, Success(res))

            case Right(\/-(-\/(push))) =>
              logger(_.debug(s"WebSocketClient received push: $push")) >>
                recvPush(push)

            case Right(-\/(err)) =>
              logger(_.error(s"WebSocketClient failed to process msg: ${BinaryData.fromArrayBuffer(msg)}\n$err")) >>
                onDecodeFailure(err)

            case Left(err) =>
              logger(_.error(s"WebSocketClient failed to process msg: ${BinaryData.fromArrayBuffer(msg)}\n$err")) >>
                onException(err)
          }
        handler.runNow()
      }

      private def onDecodeFailure(e: SafePickler.DecodingFailure): Callback = Callback {
        logger(_.error(s"Failed to parse server response: $e")).runNow()
        if (e.isLocalKnownToBeOutOfDate) {
          window.alert("Unable to understand the response from the server.\nWe've upgraded our servers since you opened this page.\nPlease reload this page to get the updates.")
          ws.close(CloseReason.clientOutOfDate)
        } else {
          ws.close(CloseReasons.parseError)
        }
      }

      private def onException(err: Throwable): Callback =
        Callback {
          logger(_.exception(err)).runNow()
          val message = Option(err.getMessage)
          unsafeCloseDueToError(message)
        }

      private def onError(e: Event): Unit = {
        val message = e.asInstanceOf[js.Dynamic].message.asInstanceOf[js.UndefOr[String]]
        unsafeCloseDueToError(message.toOption)
      }

      private def unsafeCloseDueToError(msg: Option[String]): Unit = {
        val desc = s"WebSocket error occurred.${msg.fold("")(" " + _)}"
        //console.error(desc, e)
        val reason = CloseReason(CloseCode.unhandledException, CloseReasonPhrase(desc))
        ws.close(reason)
      }

      private def onClose(e: CloseEvent): Unit = {
        onClosed(Some(CloseCode(e.code)))
      }

      private def onClosed(code: Option[CloseCode]): Unit =
        code match {
          case Some(CloseCode.`unauthorised`) =>
            state = state.copy(authorised = false)
            setPublicState(State.Unauthorised).runNow()
            connect.runNow()

          case _ =>
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
            i.ws.close(CloseReason.normalClosure)
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
            val msgBin   = protocolCS.codec.encode(msgValue)
            queueOldestToNewest :+= ((reqId, msgBin))
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
      ServerSideProcInvoker.fromSimple(send(p)(_))
  }

  private val errorClosing      = js.JavaScriptException("Connection is closing.")
  private val errorClosed       = js.JavaScriptException("Connection is closed.")
  private val errorFailed       = js.JavaScriptException("Failed to connect to server.")
  private val errorUnauthorised = js.JavaScriptException("Session expired. You must login again.")

  // ===================================================================================================================

  private trait RequestManager[Id, A, S] {
    def newRequest(state: S): CallbackTo[(Id, AsyncCallback[A])]
    def getState(id: Id): Option[S]
    def complete(id: Id, a: Try[A]): Callback
    def completeAll(t: Try[A]): CallbackTo[List[Throwable]]
    def remove(id: Id): Callback
  }

  private object RequestManager {
    private final class ArrayStore[A, S] extends RequestManager[ReqId, A, S] {
      private var prevId = 0
      private var size = 0
      private var state = new js.Array[js.UndefOr[(S, Try[A] => Callback)]]

      private def get(id: ReqId): Option[(S, Try[A] => Callback)] = {
        // console.log("GET: ", state.asInstanceOf[js.Array[js.Any]])
        val e = state(id.value)
        e.toOption
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

      private def unsafeGetAndRemove(reqId: ReqId): Option[(S, Try[A] => Callback)] = {
        val r = get(reqId)
        if (r.isDefined) {
          assert(size > 0, s"WebSocketClient.RequestManager: size=$size, reqId=${reqId.value}, state=$state")
          size -= 1
          if (size == 0)
            state = new js.Array
          else
            js.special.delete(state, reqId.value)
        }
        r
      }

      override def complete(reqId: ReqId, a: Try[A]): Callback =
        Callback {
          for ((_, f) <- unsafeGetAndRemove(reqId))
            f(a).runNow()
        }

      override def completeAll(t: Try[A]): CallbackTo[List[Throwable]] =
        CallbackTo {
          // Extract handlers
          var l = List.empty[Try[A] => Callback]
          state.forEachJs(_.foreach(l ::= _._2))

          // Clear state
          state = new js.Array
          size = 0

          // Call handlers
          l.flatMap(_(t).attempt.runNow().left.toSeq)
        }

      override def remove(reqId: ReqId): Callback =
        Callback {
          unsafeGetAndRemove(reqId)
          ()
        }
    }

    def arrayStore[A, S]: RequestManager[ReqId, A, S] =
      new ArrayStore[A, S]
  }
}
