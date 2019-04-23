package shipreq.webapp.server.app

import com.typesafe.scalalogging.StrictLogging
import javax.websocket._
import javax.websocket.server._
import scalaz.{-\/, \/-}
import shipreq.base.util.BinaryData
import shipreq.base.util.FxModule._
import shipreq.webapp.base.Urls
import shipreq.webapp.server.logic.ProjectSpaLogic._
import shipreq.webapp.server.util.WebSocketUtil
import shipreq.webapp.server.util.WebSocketUtil.UserPropsLens
import CloseReason.{CloseCode, CloseCodes}

// TODO Restrict payload size (?)
// TODO Configure timeouts
// TODO Add tracing
// TODO Info logging for messages (like other requests)

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
  }
}

@ServerEndpoint(
  value        = Urls.ProjectSpaWebSocket.ServerEndpoint,
  configurator = classOf[ProjectSpaWebSocket.Connector])
final class ProjectSpaWebSocket extends StrictLogging {
  import ProjectSpaWebSocket._

  private def close(s: Session, code: CloseCode, reasonPhrase: String): Unit =
    if (s.isOpen)
      s.close(new CloseReason(code, reasonPhrase))

  @OnOpen
  def onOpen(s: Session): Unit = {
    Option(connectRejectionL.get(s.getUserProperties)) match {
      case Some(r) =>
        logger.warn(s"Rejecting WebSocket connection: $r")
        close(s, CloseCodes.CANNOT_ACCEPT, r.toString)

      case None =>
        val userProps = s.getUserProperties
        val static    = staticL.get(userProps)
        val state     = stateL.get(userProps)
        val remote    = s.getBasicRemote
        val pushFn    = (b: BinaryData) => Fx(remote.sendBinary(b.toByteBuffer))
        val state2    = projectSpaLogic.onOpen(static, state, pushFn).unsafeRun()
        stateL.set(userProps, state2)
    }
  }

  @OnMessage
  def onMessage(s: Session, messageBytes: Array[Byte]): Unit = {
    if (messageBytes.length == 0) {
      logger.debug("Received keep-alive")
    } else {
      def msgDesc   = messageBytes.mkString("[", ",", "]")
      val userProps = s.getUserProperties
      val static    = staticL.get(userProps)
      val remote    = s.getBasicRemote
      val binIn     = BinaryData.unsafeFromArray(messageBytes)
      try {
        projectSpaLogic.onMessage(static, binIn).unsafeRun() match {

          case \/-(binOut) =>
            remote.sendBinary(binOut.toByteBuffer)

          case -\/(MsgError.DecodingFailure) =>
            logger.warn(s"Failed to decode message: $msgDesc")
            close(s, CloseCodes.PROTOCOL_ERROR, "Error parsing message")
        }

      } catch {
        case t: Throwable =>
          logger.error(s"Error occurred processing message: $msgDesc.", t)
          close(s, CustomCloseCodes.UnhandledException, "Runtime exception occurred")
      }
    }
  }

  @OnError
  def onError(s: Session, cause: Throwable): Unit = {
    logger.error("Error occurred.", cause)
    close(s, CustomCloseCodes.UnhandledException, "Runtime exception occurred")
  }

  @OnClose
  def onClose(s: Session, reason: CloseReason): Unit = {
    // println("Socket Closed: " + reason)
    val state = stateL.get(s.getUserProperties)
    projectSpaLogic.onClose(state).unsafeRun()
  }
}
