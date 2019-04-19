package shipreq.webapp.server.app

import com.typesafe.scalalogging.StrictLogging
import java.nio.ByteBuffer
import javax.websocket._
import javax.websocket.server._
import scalaz.{-\/, \/-}
import shipreq.base.util.BinaryData
import shipreq.base.util.FxModule._
import shipreq.webapp.base.Urls
import shipreq.webapp.server.logic.ProjectSpaLogic._
import shipreq.webapp.server.util.WebSocketUtil
import shipreq.webapp.server.util.WebSocketUtil.UserPropsLens

// TODO Verify Origin header
// TODO Restrict payload size (?)
// TODO Configure timeouts
// TODO Add tracing

object ProjectSpaWebSocket extends StrictLogging {

  def projectSpaLogic = Global.logic.projectSpa

  val staticL = UserPropsLens.atKey[WebSocketStatic]("X")
  val stateL  = UserPropsLens.atKey[WebSocketState ]("Y")

  final class Connector extends ServerEndpointConfig.Configurator {
    private[this] val pathPrefix = Urls.ProjectSpaWebSocket.Base.length + 1

    override def modifyHandshake(cfg: ServerEndpointConfig, req: HandshakeRequest, res: HandshakeResponse): Unit = {
      val path           = req.getRequestURI.getPath
      val projectIdParam = path.substring(pathPrefix)
      val projectId      = Urls.ProjectSpaWebSocket.parseProjectId(projectIdParam)
      val cookieLookup   = WebSocketUtil.cookieLookupFnOverHandshakeRequest(req)

      projectSpaLogic.onConnect(cookieLookup, projectId).unsafeRun() match {

        case \/-((static, state)) =>
          val userProps = cfg.getUserProperties
          staticL.set(userProps, static)
          stateL.set(userProps, state)

        case -\/(r) =>
          val msg = s"Rejecting WebSocket connection: $r"
          logger.warn(msg)
          throw new InstantiationException(msg)
          // ^^ https://stackoverflow.com/questions/25992111/how-does-serverendpointconfig-configurator-work
      }
    }
  }
}

@ServerEndpoint(
  value        = Urls.ProjectSpaWebSocket.ServerEndpoint,
  configurator = classOf[ProjectSpaWebSocket.Connector])
final class ProjectSpaWebSocket extends StrictLogging {
  import ProjectSpaWebSocket._

  @OnOpen
  def onOpen(s: Session): Unit = {
    val userProps = s.getUserProperties
    val remote    = s.getBasicRemote
    val static    = staticL.get(userProps)
    val setState  = stateL.set(userProps, _)

    val msgHandler: MessageHandler.Whole[ByteBuffer] = bb =>
      if (bb.limit() == 0) {
        logger.debug("Received keep-alive")
      } else {
        val state            = stateL.get(userProps)
        val binIn            = BinaryData.unsafeFromByteBuffer(bb)
        val (binOut, state2) = projectSpaLogic.onMessage(static)(state, binIn).unsafeRun()
        remote.sendBinary(binOut.toByteBuffer)
        state2.foreach(setState)
      }

    s.addMessageHandler(msgHandler)
  }

//  @OnClose
//  def onWebSocketClose(s: Session, reason: CloseReason): Unit = {
//    println("Socket Closed: " + reason)
//  }
//
//  @OnError
//  def onWebSocketError(cause: Throwable): Unit = {
//     cause.printStackTrace(System.err)
//  }
}
