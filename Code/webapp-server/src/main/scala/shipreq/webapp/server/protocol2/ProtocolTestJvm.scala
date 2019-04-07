package shipreq.webapp.server.protocol2

import boopickle.{PickleImpl, Pickler, UnpickleImpl}
import java.nio.ByteBuffer
import javax.websocket._
import scalaz.{\/, \/-}
import shipreq.webapp.base.Urls
import shipreq.webapp.base.protocol2._

class WebSocketSvr[Req, Push](val protocolCS: Pickler[(Int, ByteBuffer)],
                              val protocolReq: Pickler[Req],
                              val protocolSC: Pickler[Push \/ (Int, ByteBuffer)],
                              val respond   : Req => ByteBuffer) extends Endpoint {

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

        ???
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
    implicit def protocolPush: Pickler[Push] = protocol.protocolPush.codec
    new WebSocketSvr[Req, Push](protocolCS, protocolReq.codec, protocolSC, respond)
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

  import ProtocolTestJvm.sampleSvr.{protocolCS, protocolReq ,protocolSC, respond}
  private def toByteBuffer[A](p: Pickler[A])(a: A): ByteBuffer =
    PickleImpl.intoBytes(a)(implicitly, p)

  @OnOpen
  def onWebSocketConnect(s: Session): Unit = {
//    println(s"Socket Connected: $s")
  }

  @OnMessage
  def onMessage(s: Session, bb: ByteBuffer): Unit = {
//    bb.mark()
    println(s"Received message: (${bb.limit()}) ${bb.array().toList.map(_.toInt)}")
//    bb.reset()
    val recv = UnpickleImpl(protocolCS).fromBytes(bb)
    val (reqId, reqBB) = recv
    val req = UnpickleImpl(protocolReq).fromBytes(reqBB)
    println(s"RECV: " + (reqId, req))
    val res = \/-(reqId, respond(req))
    println(s"RESP: " + res)
    val resBB = toByteBuffer(protocolSC)(res)
    s.getBasicRemote.sendBinary(resBB)
  }

  @OnClose
  def onWebSocketClose(s: Session, reason: CloseReason): Unit = {
    println("Socket Closed: " + reason)
  }

  @OnError
  def onWebSocketError(cause: Throwable): Unit = {
     cause.printStackTrace(System.err)
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