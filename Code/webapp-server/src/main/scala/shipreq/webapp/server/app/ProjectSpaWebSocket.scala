package shipreq.webapp.server.app

import com.typesafe.scalalogging.StrictLogging
import javax.websocket._
import javax.websocket.server._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.BinaryData
import shipreq.base.util.FxModule._
import shipreq.webapp.base.Urls
import shipreq.webapp.server.logic.ProjectSpaLogic._
import shipreq.webapp.server.util.WebSocketUtil
import shipreq.webapp.server.util.WebSocketUtil.UserPropsLens
import CloseReason.{CloseCode, CloseCodes}

object ProjectSpaWebSocket extends StrictLogging {

  def projectSpaLogic = Global.logic.projectSpa

  val staticL           = UserPropsLens.atKey[WebSocketStatic]("X")
  val stateL            = UserPropsLens.atKey[WebSocketState[Fx]]("Y")
  val connectRejectionL = UserPropsLens.atKey[ConnectRejection]("Z")

  final class Connector extends ServerEndpointConfig.Configurator {
    private[this] val pathPrefix = Urls.ProjectSpaWebSocket.Base.length + 1

    override def modifyHandshake(cfg: ServerEndpointConfig, req: HandshakeRequest, res: HandshakeResponse): Unit = {
      val path           = req.getRequestURI.getPath
      val projectIdParam = path.substring(pathPrefix)
      val projectId      = Urls.ProjectSpaWebSocket.parseProjectId(projectIdParam)
      val cookieLookup   = WebSocketUtil.cookieLookupFnOverHandshakeRequest(req)
      val userProps      = cfg.getUserProperties

      projectSpaLogic.onConnect(cookieLookup, projectId).unsafeRun() match {

        case \/-((static, state)) =>
          staticL.set(userProps, static)
          stateL.set(userProps, state)

        case -\/(r) =>
          connectRejectionL.set(userProps, r)
      }
    }
  }

  object CustomCloseCodes {
    val UnhandledException = WebSocketUtil.CustomCloseCode(4500)
    val RespondException   = WebSocketUtil.CustomCloseCode(4501)
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

  private def fxClose(s: Session, code: CloseCode, reasonPhrase: String): Fx[Unit] =
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

  @OnOpen
  def onOpen(s: Session): Unit = {
    val startMs   = System.currentTimeMillis()
    Option(connectRejectionL.get(s.getUserProperties)) match {
      case Some(r) =>
        logger.warn(s"Rejecting WebSocket connection: $r")
        fxClose(s, CloseCodes.CANNOT_ACCEPT, r.toString).unsafeRun()

      case None =>
        val userProps = s.getUserProperties
        val static    = staticL.get(userProps)
        val state     = stateL.get(userProps)
        val state2    = projectSpaLogic.onOpen(static, state, fxPush(s)).unsafeRun()
        stateL.set(userProps, state2)
    }
    val durMs = System.currentTimeMillis() - startMs
    logger.debug(s"WebSocket ${s.getRequestURI.getPath} open completed $durMs ms")
  }

  private[this] val rightUnit = \/-(())

  @OnMessage
  def onMessage(s: Session, messageBytes: Array[Byte]): Unit = {
    if (messageBytes.length == 0) {
      logger.debug("Received keep-alive")
    } else {
      val userProps = s.getUserProperties
      val static    = staticL.get(userProps)
      val state     = stateL.get(userProps)
      val remote    = s.getBasicRemote
      val binIn     = BinaryData.unsafeFromArray(messageBytes)

      val fxSend: BinaryData => Fx[Throwable \/ Unit] =
        b => Fx {
          unsafeSend(s, b)
          rightUnit
        }

      val fxOnError: MsgError => Fx[Unit] = {
        case MsgError.DecodingFailure => fxClose(s, CloseCodes.PROTOCOL_ERROR, "Error parsing message")
        case MsgError.RespondError(_) => fxClose(s, CustomCloseCodes.RespondException, "Error sending response")
      }

      val main = projectSpaLogic.onMessage(static, state, binIn, fxSend, fxPush(s), fxOnError)

      for (state2 <- main.unsafeRun())
        stateL.set(userProps, state2)
    }
  }

  @OnError
  def onError(s: Session, cause: Throwable): Unit = {
    logger.error("Error occurred.", cause)
    fxClose(s, CustomCloseCodes.UnhandledException, "Runtime exception occurred").unsafeRun()
  }

  @OnClose
  def onClose(s: Session, reason: CloseReason): Unit = {
    val userProps = s.getUserProperties
    val static    = Option(staticL.get(userProps))
    val state     = Option(stateL.get(userProps))
    projectSpaLogic.onClose(static, state).unsafeRun()
  }
}
