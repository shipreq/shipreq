package shipreq.webapp.server.websocket

import com.typesafe.scalalogging.StrictLogging
import javax.websocket.server._
import javax.websocket.{CloseReason => _, _}
import org.slf4j.MDC
import scala.Predef.classOf
import shipreq.base.util.BinaryData
import shipreq.base.util.FxModule._
import shipreq.base.util.log.WebappLogFields
import shipreq.taskman.api.TaskmanApi
import shipreq.webapp.base.config.Urls
import shipreq.webapp.base.protocol.websocket.WebSocketShared.CloseReason
import shipreq.webapp.server.config.Global
import shipreq.webapp.server.logic.logic.ProjectSpaLogic._
import shipreq.webapp.server.protocol.websocket.WebSocketUtil
import shipreq.webapp.server.protocol.websocket.WebSocketUtil.Implicits._
import shipreq.webapp.server.protocol.websocket.WebSocketUtil.{CloseReasons, UserPropsLens}
import shipreq.webapp.server.taskman.Taskman

object ProjectSpaWebSocket extends StrictLogging {

  def projectSpaLogic = Global.logic.projectSpa

  final case class LoggingData(path: String)

  val staticL           = UserPropsLens.atKey[WebSocketStatic   ]("X")
  val stateL            = UserPropsLens.atKey[WebSocketState[Fx]]("Y")
  val connectRejectionL = UserPropsLens.atKey[ConnectRejection  ]("Z")
  val loggingDataL      = UserPropsLens.atKey[LoggingData       ]("L")
  val taskmanL          = UserPropsLens.atKey[TaskmanApi[Fx]    ]("T")

  final class Connector extends ServerEndpointConfig.Configurator {
    Urls.ProjectSpaWebSocket.Base.length + 1

    override def modifyHandshake(cfg: ServerEndpointConfig, req: HandshakeRequest, res: HandshakeResponse): Unit = {
      val userProps = cfg.getUserProperties
      val path      = req.getRequestURI.getPath

      loggingDataL.set(userProps, LoggingData(path))
      taskmanL.set(userProps, Global.taskman)

      Urls.ProjectSpaWebSocket.parsePath(path) match {
        case Some((projectId, creator)) =>

          val cookieLookup = WebSocketUtil.cookieLookupFnOverHandshakeRequest(req)

          projectSpaLogic.onConnect(cookieLookup, projectId, creator).unsafeRun() match {

            case \/-((static, state)) =>
              staticL.set(userProps, static)
              stateL.set(userProps, state)

            case -\/(r) =>
              connectRejectionL.set(userProps, r)
          }

        case None =>
          connectRejectionL.set(userProps, ConnectRejection.InvalidProjectId)
      }
    }
  }
}

@ServerEndpoint(
  value        = Urls.ProjectSpaWebSocket.ServerEndpoint,
  configurator = classOf[ProjectSpaWebSocket.Connector])
final class ProjectSpaWebSocket extends StrictLogging {
  import ProjectSpaWebSocket._

  private def unsafeSend(s: Session, b: BinaryData): Unit = {
    // Can't use s.getBasicRemote.sendBinary() - it can cause "Blocking message pending 10000 for BLOCKING" errors
    val onResult: SendHandler = r => if (!r.isOK) onError(s, r.getException)
    s.getAsyncRemote.sendBinary(b.unsafeByteBuffer, onResult)
  }

  private def fxClose(s: Session, reason: CloseReason): Fx[Unit] =
    Fx {
      try {
        if (s.isOpen)
          s.close(reason)
      } catch {
        case t: Throwable =>
          logger.error("Failed to close session.", t)
      }
    }

  private def fxPush(s: Session): BinaryData => Fx[Unit] = {
    b => Fx(unsafeSend(s, b))
  }

  private def withMdc[A](s: Session, callbackName: String)(a: => A): A = {
    val userProps   = s.getUserProperties
    val loggingData = loggingDataL.get(userProps)
    val static      = staticL.get(userProps)
    try {
      WebappLogFields.request.uri.mdcUnsafePut(loggingData.path)
      WebappLogFields.websocket.callback.mdcUnsafePut(callbackName)
      if (static ne null) {
        WebappLogFields.jwt.sessionId.mdcUnsafePut(static.sessionId.value)
        WebappLogFields.jwt.username.mdcUnsafePut(static.user.username.value)
        WebappLogFields.jwt.userId.mdcUnsafePut(static.user.id.value)
        WebappLogFields.websocket.projectId.mdcUnsafePut(static.projectId.value)
      }
      a
    } finally
      MDC.clear()
  }

  @OnOpen
  def onOpen(s: Session): Unit = withMdc(s, "open") {
    import ConnectRejection._

    val startMs = System.currentTimeMillis()
    Option(connectRejectionL.get(s.getUserProperties)) match {

      case None =>
        val userProps = s.getUserProperties
        val static    = staticL.get(userProps)
        val state     = stateL.get(userProps)
        val onError   = onListenerError(s)
        val fx        = projectSpaLogic.onOpen(static, state, fxPush(s), onError)
        val state2    = fx.unsafeRun()
        stateL.set(userProps, state2)

      case Some(NoSession | ExpiredSession) =>
        fxClose(s, CloseReasons.unauthorised).unsafeRun()

      case Some(r@ (AnonymousSession | AccessDenied | InvalidProjectId)) =>
        logger.warn(s"Rejecting WebSocket connection: $r")
        // For security reasons, don't vary the response in a way that would allow attackers to know when they've
        // discovered a valid project ID, or an existing project.
        fxClose(s, CloseReasons.invalidRequest).unsafeRun()
    }

    val durMs = System.currentTimeMillis() - startMs
    logger.debug(s"WebSocket ${s.getRequestURI.getPath} open completed $durMs ms")
  }

  private[this] val rightUnit = \/-(())

  @OnMessage
  def onMessage(s: Session, messageBytes: Array[Byte]): Unit = withMdc(s, "message") {
    val userProps = s.getUserProperties
    val static    = staticL.get(userProps)

    val fxOnMsgError: MsgError => Fx[Unit] = {
      case MsgError.SessionExpired               => fxClose(s, CloseReasons.unauthorised)
      case _: MsgError.ClientMsgDecodingFailure  => fxClose(s, CloseReasons.errorParsingMessage)
      case _: MsgError.RespondError              => fxClose(s, CloseReasons.errorSendingResponse)
      case _: MsgError.FunctionNoLongerSupported => fxClose(s, CloseReason.clientOutOfDate)
      case _: MsgError.ServerBehindClient
         | _: MsgError.ServerBehindDatabase
         | _: MsgError.ServerBehindRedis         => fxClose(s, CloseReasons.serverOutOfDate)
    }

    if (messageBytes.length == 0) {
      logger.trace("Received keep-alive")
      projectSpaLogic.onKeepAlive(static, fxOnMsgError).unsafeRun()

    } else {
      val state = stateL.get(userProps)
      val binIn = BinaryData.unsafeFromArray(messageBytes)

      val fxSend: BinaryData => Fx[Throwable \/ Unit] =
        b => Fx {
          unsafeSend(s, b)
          rightUnit
        }

      val main = projectSpaLogic.onMessage(
        static          = static,
        state           = state,
        msg             = binIn,
        respond         = fxSend,
        push            = fxPush(s),
        onListenerError = onListenerError(s),
        onError         = fxOnMsgError)

      for (state2 <- main.unsafeRun())
        stateL.set(userProps, state2)
    }
  }

  private def onListenerError(s: Session): ListenerError => Fx[Unit] = {
    case ListenerError.RedisDecodingFailure(e) =>
      if (e.isLocalKnownToBeOutOfDate)
        fxClose(s, CloseReasons.serverOutOfDate)
      else
        fxClose(s, CloseReasons.errorParsingSubscriptionData)

    case ListenerError.SessionExpired =>
      fxClose(s, CloseReasons.unauthorised)

    case ListenerError.RedisLibraryException(_) =>
      fxClose(s, CloseReasons.runtimeExceptionOccurred)
  }

  @OnError
  def onError(s: Session, cause: Throwable): Unit = withMdc(s, "error") {
    logger.error("Error occurred.", cause)

    fxClose(s, CloseReasons.runtimeExceptionOccurred).unsafeRun()

    val userProps = s.getUserProperties
    val taskman   = taskmanL.get(userProps)
    val static    = staticL.get(userProps)
    val task      = Taskman.reportServerError(Option(static.user.id), cause)
    taskman.submit(task).unsafeRun()
  }

  @OnClose
  @nowarn("cat=unused")
  def onClose(s: Session, reason: javax.websocket.CloseReason): Unit = withMdc(s, "close") {
    val userProps = s.getUserProperties
    val static    = Option(staticL.get(userProps))
    val state     = Option(stateL.get(userProps))
    projectSpaLogic.onClose(static, state).unsafeRun()
  }
}
