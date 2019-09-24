package shipreq.webapp.server.util

import javax.websocket.CloseReason.CloseCode
import javax.websocket.server._
import scala.collection.JavaConverters._
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
      case Some(cookieStr) => cookieLookupFnOverHeader(cookieStr)
      case None            => _ => None
    }

  def cookieLookupFnOverHeader(header: String): Cookie.LookupFn =
    name => {
      val k = name.value + "="

      val startIdx: Int =
        if (header.startsWith(k))
          0
        else {
          val i = header.indexOf("; " + k, k.length)
          if (i > 0) i + 2 else -1
        }

      if (startIdx < 0)
        None
      else
        Some(header.substring(startIdx + k.length).takeWhile(_ != ';'))
    }

  // Doesn't work -- https://github.com/eclipse/jetty.project/issues/3575
//  def pathParam(req: HandshakeRequest, name: String): String =
//    req.getParameterMap.get(name).asScala.headOption.getOrElse {
//      throw new IllegalStateException(s"WebSocket PathParam '$name' not found. uri=${req.getRequestURI}")
//    }

  class CustomCloseCode(code: Int) extends CloseCode {
    override final def getCode = code
  }

  object CustomCloseCode {
    def apply(code: Int): CustomCloseCode =
      new CustomCloseCode(code)
  }
}