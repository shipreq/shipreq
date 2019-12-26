package shipreq.webapp.server.app

import com.typesafe.scalalogging.StrictLogging
import javax.websocket._
import javax.websocket.server._
import org.slf4j.MDC
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.BinaryData
import shipreq.base.util.FxModule._
import shipreq.base.util.log.WebappLogFields
import shipreq.webapp.base.Urls
import shipreq.webapp.base.protocol.WebSocketShared.CloseCode
import shipreq.webapp.server.logic.ProjectSpaLogic._
import shipreq.webapp.server.util.WebSocketUtil
import shipreq.webapp.server.util.WebSocketUtil.UserPropsLens

object ProjectSpaWebSocket extends StrictLogging {

  def projectSpaLogic = Global.logic.projectSpa

  final case class LoggingData(path: String)

  val staticL           = UserPropsLens.atKey[WebSocketStatic   ]("X")
  val stateL            = UserPropsLens.atKey[WebSocketState[Fx]]("Y")
  val connectRejectionL = UserPropsLens.atKey[ConnectRejection  ]("Z")
  val loggingDataL      = UserPropsLens.atKey[LoggingData       ]("L")

  final class Connector extends ServerEndpointConfig.Configurator {
    private[this] val pathPrefix = Urls.ProjectSpaWebSocket.Base.length + 1

    override def modifyHandshake(cfg: ServerEndpointConfig, req: HandshakeRequest, res: HandshakeResponse): Unit = {
      val path           = req.getRequestURI.getPath
      val projectIdParam = path.substring(pathPrefix)
      val projectId      = Urls.ProjectSpaWebSocket.parseProjectId(projectIdParam)
      val cookieLookup   = WebSocketUtil.cookieLookupFnOverHandshakeRequest(req)
      val userProps      = cfg.getUserProperties

      loggingDataL.set(userProps, LoggingData(path))

      projectSpaLogic.onConnect(cookieLookup, projectId).unsafeRun() match {

        case \/-((static, state)) =>
          staticL.set(userProps, static)
          stateL.set(userProps, state)

        case -\/(r) =>
          connectRejectionL.set(userProps, r)
      }
    }
  }
}

@ServerEndpoint(
  value        = Urls.ProjectSpaWebSocket.ServerEndpoint,
  configurator = classOf[ProjectSpaWebSocket.Connector])
final class ProjectSpaWebSocket extends StrictLogging {
  import CloseReason.CloseCodes
  import ProjectSpaWebSocket._

  private def unsafeSend(s: Session, b: BinaryData): Unit = {
    // Can't use s.getBasicRemote.sendBinary() - it can cause "Blocking message pending 10000 for BLOCKING" errors
    val onResult: SendHandler = r => if (!r.isOK) onError(s, r.getException)
    s.getAsyncRemote.sendBinary(b.unsafeByteBuffer, onResult)
  }

  private def fxClose(s: Session, code: CloseCode, reasonPhrase: String): Fx[Unit] =
    fxClose(s, WebSocketUtil.CustomCloseCode(code.value), reasonPhrase)

  private def fxClose(s: Session, code: CloseReason.CloseCode, reasonPhrase: String): Fx[Unit] =
    Fx {
      try {
        if (s.isOpen)
          s.close(new CloseReason(code, reasonPhrase))
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
    val startMs = System.currentTimeMillis()
    Option(connectRejectionL.get(s.getUserProperties)) match {
      case Some(r) =>
        logger.warn(s"Rejecting WebSocket connection: $r")
        fxClose(s, CloseCodes.CANNOT_ACCEPT, r.toString).unsafeRun()

      case None =>
        val userProps = s.getUserProperties
        val static    = staticL.get(userProps)
        val state     = stateL.get(userProps)
        val onError   = onListenerError(s)
        val fx        = projectSpaLogic.onOpen(static, state, fxPush(s), onError)
        val state2    = fx.unsafeRun()
        stateL.set(userProps, state2)
    }
    val durMs = System.currentTimeMillis() - startMs
    logger.debug(s"WebSocket ${s.getRequestURI.getPath} open completed $durMs ms")
  }

  private[this] val rightUnit = \/-(())

  @OnMessage
  def onMessage(s: Session, messageBytes: Array[Byte]): Unit = withMdc(s, "message") {
    if (messageBytes.length == 0) {
      logger.trace("Received keep-alive")
    } else {
      val userProps = s.getUserProperties
      val static    = staticL.get(userProps)
      val state     = stateL.get(userProps)
      val binIn     = BinaryData.unsafeFromArray(messageBytes)

      val fxSend: BinaryData => Fx[Throwable \/ Unit] =
        b => Fx {
          unsafeSend(s, b)
          rightUnit
        }

      val fxOnMsgError: MsgError => Fx[Unit] = {
        case _: MsgError.ClientMsgDecodingFailure => fxClose(s, CloseCodes.PROTOCOL_ERROR, "Error parsing message")
        case _: MsgError.RespondError             => fxClose(s, CloseCode.RespondException, "Error sending response")
        case _: MsgError.ServerBehindClient
           | _: MsgError.ServerBehindDatabase
           | _: MsgError.ServerBehindRedis        => fxClose(s, CloseCodes.SERVICE_RESTART, "Server is out-of-date")
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
        fxClose(s, CloseCodes.SERVICE_RESTART, "Server is out-of-date")
      else
        fxClose(s, CloseCodes.UNEXPECTED_CONDITION, "Error parsing subscription data")
  }

  @OnError
  def onError(s: Session, cause: Throwable): Unit = withMdc(s, "error") {
    logger.error("Error occurred.", cause)
    fxClose(s, CloseCode.UnhandledException, "Runtime exception occurred").unsafeRun()
  }

  @OnClose
  def onClose(s: Session, reason: CloseReason): Unit = withMdc(s, "close") {
    val userProps = s.getUserProperties
    val static    = Option(staticL.get(userProps))
    val state     = Option(stateL.get(userProps))
    projectSpaLogic.onClose(static, state).unsafeRun()
  }
}
