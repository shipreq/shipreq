package shipreq.webapp.server.logic

import doobie.imports.ConnectionIO
import java.nio.ByteBuffer
import java.time.Duration
import scalaz.syntax.monad._
import scalaz.{-\/, Monad, \/-, ~>}
import shipreq.base.ops.Trace._
import shipreq.base.util.Url

object TraceLogic {

  object AttrFor {
    def none[A]: AttrFor[A] =
      _ => Nil

    val sspRequest: List[Attr] =
      Attr.HttpMethod("POST") :: Nil

    val sspResponseOk: List[Attr] =
      Attr.HttpStatus200 :: Nil

    val sspResponse: AttrFor[Server.SspResponse] = {
      case \/-(output)                        => Attr.HttpResponseSize(output.limit) :: sspResponseOk
      case -\/(Server.RequestPickleError(e))  => Attr.HttpStatusCode(400) :: Attr.Error(e) :: Nil
      case -\/(Server.ResponsePickleError(e)) => Attr.HttpStatusCode(500) :: Attr.Error(e) :: Nil
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Logic {
    def off[F[_], HttpReq, HttpRes](implicit F: Monad[F]): Logic[F, HttpReq, HttpRes] =
      new Logic[F, HttpReq, HttpRes]()(F, Algebra.off, AttrFor.none, AttrFor.none)
  }

  final class Logic[F[_], HttpReq, HttpRes](implicit F: Monad[F],
                                            val alg: Algebra[F],
                                            attrForHttpReq: AttrFor[HttpReq],
                                            attrForHttpRes: AttrFor[HttpRes]) {
    type Span = alg.Span
    import alg._

    /** Trace a HTTP request.
      *
      * Creates a top-level trace.
      */
    def http(routeName: String, req: HttpReq, path: Url.Relative)
            (respond: Span => F[HttpRes]): F[HttpRes] =
      newSpan("HTTP: " + routeName) { implicit span =>
        for {
          _   <- addAttrs(Attr.EndpointName(routeName) :: Attr.HttpUri(path.relativeUrl) :: attrForHttpReq(req))
          res <- respond(span)
          _   <- addAttrs(attrForHttpRes(res))
        } yield res
      }

    /** Trace the invocation of a server-side procedure (by the user's browser).
      *
      * Creates a top-level trace.
      */
    def serverSideProc(sspName: String, parent: Span, input: ByteBuffer)
                      (respond: Span => F[Server.SspResponse]): F[Server.SspResponse] =
      // Making this a child span ruins the times of both the initial HTTP request, and the AJAX call when searching and
      // filtering....which is ok.
      // Use metrics system for timings of HTTP/AJAX, tracing is for debugging and/or perspective of higher-level
      // processes...
      newSubSpan("AJAX: " + sspName, parent) { implicit span =>
        for {
          _ <- addAttrs(Attr.EndpointName(sspName) :: Attr.HttpRequestSize(input.limit) :: AttrFor.sspRequest)
          r <- respond(span)
          _ <- addAttrs(AttrFor.sspResponse(r))
        } yield r
      }

    def injectDb(real: ConnectionIO ~> F): ConnectionIO ~> F =
      new (ConnectionIO ~> F) {
        override def apply[A](fa: ConnectionIO[A]): F[A] =
          newSpan("Doobie")(_ => real(fa))
      }

    def injectServer(orig: Server.Algebra[F]): Server.Algebra[F] =
      new Server.Delegate(orig) {
        override def fork[A](fa: F[A]) =
          newSpan("fork")(Function const propagateCtx[A].flatMap(f => orig fork f(fa)))

        override val registerServerSideProc = (name, f) =>
          newSpan("RegisterSSP") { implicit span =>
            addAttrs(Attr.EndpointName(name) :: Nil) >>
            orig.registerServerSideProc(name, i =>
              serverSideProc(name, span, i)(_ =>
                f(i)))
          }
      }

  }

}
