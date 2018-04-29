package shipreq.webapp.server.app

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.LiftResponse
import net.liftweb.http.provider.servlet.HTTPRequestServlet
import shipreq.base.ops.Trace._
import shipreq.webapp.server.logic.TraceLogic

object TraceInterpreter {

  type HttpReq = net.liftweb.http.Req
  type HttpRes = Box[LiftResponse]

  type ForLift[F[_]] = TraceLogic[F, HttpReq, HttpRes]

  object Implicits {

    implicit val attrForHttpReq: AttrFor[HttpReq] =
      req => {
        val r2 = req.request
        var attrs =
          Attr.HttpMethod(req.requestType.method) ::
            Attr.HttpUrl(r2.url) ::
            Attr.HttpRemoteHost(r2.remoteHost) ::
            Attr.HttpRemotePort(r2.remotePort) ::
            req.userAgent.toList.map(Attr.HttpUserAgent)
        r2 match {
          case x: HTTPRequestServlet => attrs ::= Attr.HttpRequestSize(x.req.getContentLengthLong)
          case _                     => ()
        }
        r2.sessionId match {
          case b: Full[String] => attrs ::= Attr.HttpSessionId(b.value)
          case _               => ()
        }
        attrs
      }

    implicit val attrForHttpRes: AttrFor[HttpRes] = {
      case f: Full[LiftResponse] =>
        val r = f.value.toResponse
        Attr.httpStatusCode(r.code) ::
          Attr.HttpResponseSize(r.size) ::
          Nil
      case x: Failure =>
        Attr.httpStatusCode(500) ::
          x.rootExceptionCause.toList.map(Attr.Error)
      case Empty =>
        Nil
    }

  }
}
