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

// TODO Verify Origin header
// TODO Restrict payload size (?)
// TODO Configure timeouts
// TODO Add tracing
// TODO Info logging for messages (like other requests)

object ProjectSpaWebSocket extends StrictLogging {

  def projectSpaLogic = Global.logic.projectSpa

  val staticL           = UserPropsLens.atKey[WebSocketStatic]("X")
  val stateL            = UserPropsLens.atKey[WebSocketState]("Y")
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
    for (r <- Option(connectRejectionL.get(s.getUserProperties))) {
      logger.warn(s"Rejecting WebSocket connection: $r")
      close(s, CloseCodes.CANNOT_ACCEPT, r.toString)
    }
  }

  @OnMessage
  def onMessage(s: Session, messageBytes: Array[Byte]): Unit = {
    if (messageBytes.length == 0) {
      logger.debug("Received keep-alive")
    } else {
      val userProps = s.getUserProperties
      val remote    = s.getBasicRemote
      val static    = staticL.get(userProps)
      val setState  = stateL.set(userProps, _)
      val state     = stateL.get(userProps)
      val binIn     = BinaryData.unsafeFromArray(messageBytes)
      try {
        projectSpaLogic.onMessage(static)(state, binIn).unsafeRun() match {

          case \/-((binOut, state2)) =>
            remote.sendBinary(binOut.toByteBuffer)
            state2.foreach(setState)

          case -\/(MsgError.DecodingFailure) =>
            logger.warn(s"Error parsing message: ${messageBytes.mkString("[", ",", "]")}")
            close(s, CloseCodes.PROTOCOL_ERROR, "Error parsing message")
        }

      } catch {
        case t: Throwable =>
          logger.error("Error processing message.", t)
          close(s, CustomCloseCodes.UnhandledException, "Runtime exception occurred")
      }
    }
  }

//  @OnError
//  def onWebSocketError(s: Session, cause: Throwable): Unit = {
//     cause.printStackTrace(System.err)
//    s.close(CloseReason.CloseCodes.PROTOCOL_ERROR)
//  }

//  @OnClose
//  def onWebSocketClose(s: Session, reason: CloseReason): Unit = {
//    println("Socket Closed: " + reason)
//  }
}
