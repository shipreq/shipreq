package shipreq.webapp.server.interpreter

import net.liftweb.common.Full
import net.liftweb.http.provider.servlet.HTTPRequestServlet
import shipreq.base.ops.Trace._
import shipreq.webapp.server.logic.algebra.TraceAlgebra
import shipreq.webapp.server.logic.dispatch.{Response, ResponseCmd}

object TraceInterpreter {

  type HttpReq = net.liftweb.http.Req

  type ForHttp[F[_]] = TraceAlgebra[F, HttpReq, Response]

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

    implicit val attrForHttpRes: AttrFor[Response] =
      _.cmd match {
        case _: ResponseCmd.ServePublicSpa
           | _: ResponseCmd.ServeHomeSpa
           | _: ResponseCmd.ProjectSpa.Serve
           |    ResponseCmd.ProjectAccessRevoked
        =>
          Attr.HttpStatus200 :: Nil

        case _: ResponseCmd.Redirect
           | ResponseCmd.ProjectSpa.AccessDenied
        =>
          Attr.HttpStatus302 :: Nil

        case c: ResponseCmd.StatusOnly =>
          Attr.httpStatusCode(c.status) :: Attr.HttpResponseSize(0) :: Nil

        case c: ResponseCmd.Text =>
          Attr.httpStatusCode(c.status) :: Attr.HttpResponseSize(c.body.length) :: Nil

        case c: ResponseCmd.Json =>
          Attr.httpStatusCode(c.status) :: Attr.HttpResponseSize(c.body.length) :: Nil

        case c: ResponseCmd.Binary =>
          Attr.httpStatusCode(c.status) :: Attr.HttpResponseSize(c.body.length) :: Nil
      }
  }
}
