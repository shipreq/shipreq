package shipreq.webapp.server.logic

import doobie.imports.ConnectionIO
import scalaz.syntax.monad._
import scalaz.{-\/, Monad, \/-, ~>}
import shipreq.base.ops.Trace
import shipreq.base.ops.Trace.{Attr, AttrFor}
import shipreq.base.util.Url

final class TraceLogic[F[_], HttpReq, HttpRes](implicit F: Monad[F],
                                               val alg: Trace.Algebra[F],
                                               attrForHttpReq: AttrFor[HttpReq],
                                               attrForHttpRes: AttrFor[HttpRes]) {
  import TraceLogic.AttrFor

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

  def injectDb(real: ConnectionIO ~> F): ConnectionIO ~> F =
    new (ConnectionIO ~> F) {
      override def apply[A](fa: ConnectionIO[A]): F[A] =
        newSpan("Doobie")(_ => real(fa))
    }

  def injectServer(orig: Server.Algebra[F]): Server.Algebra[F] =
    new Server.Delegate(orig) {
      override def fork[A](fa: F[A]) =
        newSpan("fork")(Function const propagateCtx[A].flatMap(f => orig fork f(fa)))
    }
}

object TraceLogic {

  object AttrFor {
    def none[A]: AttrFor[A] =
      _ => Nil
  }

  def off[F[_], HttpReq, HttpRes](implicit F: Monad[F]): TraceLogic[F, HttpReq, HttpRes] =
    new TraceLogic[F, HttpReq, HttpRes]()(F, Trace.Algebra.off, AttrFor.none, AttrFor.none)
}
