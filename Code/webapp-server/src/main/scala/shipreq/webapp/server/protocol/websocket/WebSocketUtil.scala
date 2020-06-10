package shipreq.webapp.server.protocol.websocket

import javax.websocket.server._
import scala.jdk.CollectionConverters._
import shipreq.webapp.base.protocol.websocket.WebSocketShared
import shipreq.webapp.server.logic.dispatch.Cookie

object WebSocketUtil {

  type UserProps = java.util.Map[String, AnyRef]

  final case class UserPropsLens[A](get: UserProps => A, set: (UserProps, A) => Unit)

  object UserPropsLens {

    def atKey[A <: AnyRef](key: String): UserPropsLens[A] =
      new UserPropsLens[A](
        _.get(key).asInstanceOf[A],
        _.put(key, _))

    def atKey[A <: AnyRef](key: String, default: => A): UserPropsLens[A] =
      new UserPropsLens[A](
        p => {val v = p.get(key); if (v eq null) default else v.asInstanceOf[A]},
        _.put(key, _))
  }

  def cookieLookupFnOverHandshakeRequest(req: HandshakeRequest): Cookie.LookupFn =
    req.getHeaders.get("cookie").asScala.headOption match {
      case Some(cookieStr) => Cookie.LookupFn.overHeader(cookieStr)
      case None            => _ => None
    }

  // Doesn't work -- https://github.com/eclipse/jetty.project/issues/3575
//  def pathParam(req: HandshakeRequest, name: String): String =
//    req.getParameterMap.get(name).asScala.headOption.getOrElse {
//      throw new IllegalStateException(s"WebSocket PathParam '$name' not found. uri=${req.getRequestURI}")
//    }

  object CloseReasons {
    import WebSocketShared._

    val errorParsingMessage          = CloseReason(CloseCode.protocolError,       CloseReasonPhrase("Error parsing message"))
    val errorParsingSubscriptionData = CloseReason(CloseCode.unexpectedCondition, CloseReasonPhrase("Error parsing subscription data"))
    val errorSendingResponse         = CloseReason(CloseCode.respondException,    CloseReasonPhrase("Error sending response"))
    val invalidRequest               = CloseReason(CloseCode.cannotAccept,        CloseReasonPhrase("Invalid request"))
    val runtimeExceptionOccurred     = CloseReason(CloseCode.unhandledException,  CloseReasonPhrase("Runtime exception occurred"))
    val serverOutOfDate              = CloseReason(CloseCode.serviceRestart,      CloseReasonPhrase("Server is out-of-date"))
    val unauthorised                 = CloseReason(CloseCode.unauthorised,        CloseReasonPhrase("Unauthorised"))
  }

  class CustomCloseCode(code: Int) extends javax.websocket.CloseReason.CloseCode {
    override final def getCode = code
  }

  object CustomCloseCode {
    def apply(code: Int): CustomCloseCode =
      new CustomCloseCode(code)
  }

  object Implicits {
    implicit def closeReasonToJavax(r: WebSocketShared.CloseReason): javax.websocket.CloseReason =
      new javax.websocket.CloseReason(CustomCloseCode(r.code.value), r.phrase.value)
  }
}