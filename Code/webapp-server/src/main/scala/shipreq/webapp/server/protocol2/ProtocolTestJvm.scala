package shipreq.webapp.server.protocol2

import boopickle.{PickleImpl, Pickler, UnpickleImpl}
import java.nio.ByteBuffer
import javax.websocket._
import scalaz.{\/, \/-}
import shipreq.webapp.base.Urls
import shipreq.webapp.base.protocol2._

class WebSocketSvr[Req, Push](protocolCS: Pickler[(Int, Req)],
                              protocolSC: Pickler[Push \/ (Int, ByteBuffer)],
                              respond   : Req => ByteBuffer) extends Endpoint {

  private def toByteBuffer[A](p: Pickler[A])(a: A): ByteBuffer =
    PickleImpl.intoBytes(a)(implicitly, p)

  override def onOpen(session: Session, config: EndpointConfig): Unit = {
    // TODO Don't forget auth
    // TODO Also verify Origin header
    // TODO Restrict payload size
    // TODO Disable timeouts
    val remote = session.getBasicRemote

    session.addMessageHandler(new MessageHandler.Whole[ByteBuffer] {
      override def onMessage(message: ByteBuffer): Unit = {

        val recv = UnpickleImpl(protocolCS).fromBytes(message)
        println(s"RECV 1: " + recv)
        val (reqId, req) = recv
        val res = \/-(reqId, respond(req))
        println(s"RECV 2: " + res)
        val resBB = toByteBuffer(protocolSC)(res)
        remote.sendBinary(resBB)
      }
    })
  }

  override def onClose(session: Session, closeReason: CloseReason): Unit =
    super.onClose(session, closeReason)

  override def onError(session: Session, thr: Throwable): Unit =
    super.onError(session, thr)
}

object WebSocketSvr {
  def apply(protocol: Protocol.WebSocket.ClientReqServerPush[Pickler])
           (respond : protocol.Req => ByteBuffer): WebSocketSvr[protocol.Req, protocol.Push] = {
    import WebSocketShared._
    import protocol._
    implicit def protocolReq: Pickler[Req] = protocol.protocolReq.codec
    implicit def protocolPush: Pickler[Push] = protocol.protocolPush.codec
    new WebSocketSvr[Req, Push](protocolCS, protocolSC, respond)
  }
}

object ProtocolTestJvm {
  val sampleSvr = WebSocketSvr(SampleProtocol.WS)(SampleProtocol.respond)
}

// =====================================================================================================================


import javax.websocket._
import javax.websocket.server.ServerEndpoint

@ServerEndpoint(value = Urls._projectSpaWebSocket)
class EventSocket {

  @OnOpen
  def onWebSocketConnect(s: Session): Unit = {
    println(s"Socket Connected: $s")
  }

  @OnMessage
  def onMessage(s: Session, bb: ByteBuffer): Unit = {
    println(s"Received message: $bb")
  }

  @OnClose
  def onWebSocketClose(s: Session, reason: CloseReason): Unit = {
    println("Socket Closed: " + reason)
  }

  @OnError
  def onWebSocketError(cause: Throwable): Unit = {
    println("Socket error: " + cause)
    // cause.printStackTrace(System.err)
  }
}
/*
//import org.eclipse.jetty.websocket.servlet.WebSocketServlet
//import org.eclipse.jetty.websocket.servlet.WebSocketServletFactory


import org.eclipse.jetty.websocket.servlet.WebSocketServlet
import org.eclipse.jetty.websocket.servlet.WebSocketServletFactory
import javax.servlet.annotation.WebServlet

@SuppressWarnings(Array("serial"))
@WebServlet(name = "MyEcho WebSocket Servlet", urlPatterns = Array("/ws-test2"))
class MyEchoServlet extends WebSocketServlet {
  override def configure(factory: WebSocketServletFactory): Unit = { // set a 10 second timeout
    factory.getPolicy.setIdleTimeout(10000)
    // register MyEchoSocket as the WebSocket to create on Upgrade
    factory.register(classOf[EchoSocket])
  }
}


@org.eclipse.jetty.websocket.api.annotations.WebSocket
class EchoSocket {
  @org.eclipse.jetty.websocket.api.annotations.OnWebSocketConnect
  def onOpen(c: org.eclipse.jetty.websocket.api.Session): Unit = {
    println("AAAAAAAAAAAHHHHHHHHHHHHHHH")
  }
}
*/