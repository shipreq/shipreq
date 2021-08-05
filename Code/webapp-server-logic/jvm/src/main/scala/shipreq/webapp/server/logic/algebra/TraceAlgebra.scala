package shipreq.webapp.server.logic.algebra

import cats.syntax.all._
import cats.{Monad, ~>}
import doobie.ConnectionIO
import shipreq.base.ops.Trace
import shipreq.base.ops.Trace.{Attr, AttrFor}
import shipreq.base.util.Url

trait TraceAlgebra[F[_], HttpReq, HttpRes] {

  val alg: Trace.Algebra[F]

  final type Span = alg.Span

  /** Trace a HTTP request.
    *
    * Creates a top-level trace.
    */
  def http(routeName: String, req: HttpReq, path: Url.Relative)(respond: Span => F[HttpRes]): F[HttpRes]

  /** Trace the invocation of a server-side procedure (by the user's browser).
    *
    * Creates a top-level trace.
    */
  def serverSideProc(sspName: String, req: HttpReq, path: Url.Relative)(respond: Span => F[HttpRes]): F[HttpRes]

  def injectDb(real: ConnectionIO ~> F): ConnectionIO ~> F

  def injectServer(orig: Server.Algebra[F]): Server.Algebra[F]

  def injectRedis(orig: Redis.ProjectAlgebra[F]): Redis.ProjectAlgebra[F]
}


object TraceAlgebra {

  def off[F[_]: Monad, HttpReq, HttpRes]: TraceAlgebra[F, HttpReq, HttpRes] =
    new TraceAlgebra[F, HttpReq, HttpRes] {
      override val alg = Trace.Algebra.off
      override def http          (a: String, b: HttpReq, c: Url.Relative)(f: Span => F[HttpRes]) = f(())
      override def serverSideProc(a: String, b: HttpReq, c: Url.Relative)(f: Span => F[HttpRes]) = f(())
      override def injectDb      (f: ConnectionIO ~> F)                                          = f
      override def injectServer  (f: Server.Algebra[F])                                          = f
      override def injectRedis   (f: Redis.ProjectAlgebra[F])                                    = f
    }

  def on[F[_], HttpReq, HttpRes](implicit F: Monad[F],
                                 _alg: Trace.Algebra[F],
                                 attrForHttpReq: AttrFor[HttpReq],
                                 attrForHttpRes: AttrFor[HttpRes]): TraceAlgebra[F, HttpReq, HttpRes] =
    new TraceAlgebra[F, HttpReq, HttpRes] {
      import _alg.{Span => _, _}

      override val alg: _alg.type = _alg

      override def http(routeName: String, req: HttpReq, path: Url.Relative)(respond: Span => F[HttpRes]): F[HttpRes] =
        newSpan("HTTP: " + routeName) { implicit span =>
          for {
            _   <- addAttrs(Attr.EndpointName(routeName) :: Attr.HttpUri(path.relativeUrl) :: attrForHttpReq(req))
            res <- respond(span)
            _   <- addAttrs(attrForHttpRes(res))
          } yield res
        }

      override def serverSideProc(sspName: String, req: HttpReq, path: Url.Relative)(respond: Span => F[HttpRes]): F[HttpRes] =
        newSpan("AJAX: " + sspName) { implicit span =>
          for {
            _   <- addAttrs(Attr.EndpointName(sspName) :: Attr.HttpUri(path.relativeUrl) :: attrForHttpReq(req))
            res <- respond(span)
            _   <- addAttrs(attrForHttpRes(res))
          } yield res
        }

      override def injectDb(real: ConnectionIO ~> F): ConnectionIO ~> F =
        new (ConnectionIO ~> F) {
          override def apply[A](fa: ConnectionIO[A]): F[A] =
            newSpan("Doobie")(_ => real(fa))
        }

      override def injectServer(orig: Server.Algebra[F]): Server.Algebra[F] =
        new Server.Delegate(orig) {
          override def fork[A](fa: F[A]) =
            newSpan("fork")(Function const propagateCtx[A].flatMap(f => orig fork f(fa)))
        }

      override def injectRedis(orig: Redis.ProjectAlgebra[F]): Redis.ProjectAlgebra[F] =
        Redis.traced(orig, alg)
    }

  object AttrFor {
    def none[A]: AttrFor[A] =
      _ => Nil
  }
}
