package shipreq.webapp.server.logic

import doobie.imports.ConnectionIO
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import java.nio.ByteBuffer
import java.time.Duration
import scalaz.syntax.monad._
import scalaz.{-\/, Monad, \/-, ~>}
import shipreq.base.util.Url

object Trace {

  /** Unit of metadata that can be attributed to trace spans.
    *
    * @param tag Optimisation inspired by scalaz.io8. Because this is such a hot path, rather than doing a pattern-match
    *            of attributes, we assign each subclass a number and use a single int with a lookup table. This avoids
    *            many isInstanceOf calls under-the-hood and results in better performance.
    */
  sealed abstract class Attr(final private[Attr] val tag: Int)
  object Attr {

    private[this] object Tag {
      private[this] var _size = 0
      private[this] def assign(): Int = {val i = _size; _size += 1; i}

      // One tag per Attr type
      val ShipReqUserId   : Int = assign()
      val ShipReqProjectId: Int = assign()
      val EndpointName    : Int = assign()
      val HttpMethod      : Int = assign()
      val HttpUri         : Int = assign()
      val HttpUrl         : Int = assign()
      val HttpUserAgent   : Int = assign()
      val HttpStatusCode  : Int = assign()
      val HttpRequestSize : Int = assign()
      val HttpResponseSize: Int = assign()
      val Error           : Int = assign()
    }

    // Attributes
    final case class ShipReqUserId   (value: Long)      extends Attr(Tag.ShipReqUserId)
    final case class ShipReqProjectId(value: Long)      extends Attr(Tag.ShipReqProjectId)
    final case class EndpointName    (value: String)    extends Attr(Tag.EndpointName)
    final case class HttpMethod      (value: String)    extends Attr(Tag.HttpMethod)
    final case class HttpUri         (value: String)    extends Attr(Tag.HttpUri)
    final case class HttpUrl         (value: String)    extends Attr(Tag.HttpUrl)
    final case class HttpUserAgent   (value: String)    extends Attr(Tag.HttpUserAgent)
    final case class HttpStatusCode  (value: Long)      extends Attr(Tag.HttpStatusCode) {val str = value.toString}
    final case class HttpRequestSize (value: Long)      extends Attr(Tag.HttpRequestSize)
    final case class HttpResponseSize(value: Long)      extends Attr(Tag.HttpResponseSize)
    final case class Error           (value: Throwable) extends Attr(Tag.Error)

    val HttpStatus200 = HttpStatusCode(200)

    def httpStatusCode(code: Int): HttpStatusCode =
      if (code == 200) HttpStatus200 else HttpStatusCode(code)

    private val exampleValues: NonEmptyVector[Attr] =
      AdtMacros.adtValuesManually[Attr](
        ShipReqUserId   (0),
        ShipReqProjectId(0),
        EndpointName    (""),
        HttpMethod      (""),
        HttpUri         (""),
        HttpUrl         (""),
        HttpUserAgent   (""),
        HttpStatusCode  (0),
        HttpRequestSize (0),
        HttpResponseSize(0),
        Error           (null))

    def interpret[S, A](
        shipReqUserId   : (S, ShipReqUserId   ) => A,
        shipReqProjectId: (S, ShipReqProjectId) => A,
        endpointName    : (S, EndpointName    ) => A,
        httpMethod      : (S, HttpMethod      ) => A,
        httpUri         : (S, HttpUri         ) => A,
        httpUrl         : (S, HttpUrl         ) => A,
        httpUserAgent   : (S, HttpUserAgent   ) => A,
        httpStatusCode  : (S, HttpStatusCode  ) => A,
        httpRequestSize : (S, HttpRequestSize ) => A,
        httpResponseSize: (S, HttpResponseSize) => A,
        error           : (S, Error           ) => A): (S, Attr) => A = {
      type F = (S, Attr) => A
      val fns = Array.fill[F](exampleValues.length)(null)
      def addFn(f: Attr => (S, _ <: Attr) => A)(a: Attr): Unit = {
        assert(fns(a.tag) eq null, s"Duplicate Trace.Attr.tag detected! ($a)")
        fns(a.tag) = f(a).asInstanceOf[F]
      }
      exampleValues.foreach(addFn {
        case _: ShipReqUserId    => shipReqUserId
        case _: ShipReqProjectId => shipReqProjectId
        case _: EndpointName     => endpointName
        case _: HttpMethod       => httpMethod
        case _: HttpUri          => httpUri
        case _: HttpUrl          => httpUrl
        case _: HttpUserAgent    => httpUserAgent
        case _: HttpStatusCode   => httpStatusCode
        case _: HttpRequestSize  => httpRequestSize
        case _: HttpResponseSize => httpResponseSize
        case _: Error            => error
      })
      // Result:
      (s, a) => fns(a.tag)(s, a)
    }
  }

  trait AttrFor[A] {
    def apply(data: A): List[Attr]
  }

  object AttrFor {
    def none[A]: AttrFor[A] =
      _ => Nil

    val sspRequest: List[Attr] =
      Attr.HttpMethod("POST") :: Nil

    val sspResponseOk: List[Attr] =
      Attr.HttpStatus200 :: Nil

    val sspResponse: Trace.AttrFor[Server.SspResponse] = {
      case \/-(output)                        => Attr.HttpResponseSize(output.limit) :: sspResponseOk
      case -\/(Server.RequestPickleError(e))  => Attr.HttpStatusCode(400) :: Attr.Error(e) :: Nil
      case -\/(Server.ResponsePickleError(e)) => Attr.HttpStatusCode(500) :: Attr.Error(e) :: Nil
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  abstract class Algebra[F[_]](implicit F: Monad[F]) { outer =>
    type Span

    def newSpan[A](name: String)(f: Span => F[A]): F[A]
    def newSubSpan[A](name: String, parent: Span)(f: Span => F[A]): F[A]
    def addAttrs(attrs: List[Trace.Attr])(implicit span: Span): F[Unit]

    def compose(inner: Algebra[F]): Algebra[F] =
      new Algebra[F] {
        override type Span = (outer.Span, inner.Span)
        override def newSpan[A](name: String)(f: Span => F[A]): F[A] =
          outer.newSpan(name)(ospan =>
            inner.newSpan(name)(ispan =>
              f((ospan, ispan))))
        override def newSubSpan[A](name: String, parent: Span)(f: Span => F[A]): F[A] =
          outer.newSubSpan(name, parent._1)(ospan =>
            inner.newSubSpan(name, parent._2)(ispan =>
              f((ospan, ispan))))
        override def addAttrs(attrs: List[Trace.Attr])(implicit span: Span): F[Unit] =
          outer.addAttrs(attrs)(span._1) >> inner.addAttrs(attrs)(span._2)
      }
  }

  object Algebra {
    def apply[F[_]: Monad](algebras: TraversableOnce[Algebra[F]]): Algebra[F] =
      if (algebras.isEmpty)
        off
      else
        algebras.reduce(_ compose _)

    def off[F[_]](implicit F: Monad[F]): Algebra[F] =
      new Algebra[F] {
        val fUnit = F.point(())
        override type Span                                              = Unit
        override def newSpan[A](n: String)(f: Span => F[A])             = f(())
        override def newSubSpan[A](n: String, p: Span)(f: Span => F[A]) = f(())
        override def addAttrs(a: List[Trace.Attr])(implicit span: Span) = fUnit
        override def compose(a: Algebra[F])                             = a
      }

    def logToStdout[F[_]](implicit F: Monad[F]): Algebra[F] =
      new Algebra[F] {
        val fUnit = F.point(())
        override type Span                                              = Unit
        override def newSpan[A](n: String)(f: Span => F[A])             = this(n, f)
        override def newSubSpan[A](n: String, p: Span)(f: Span => F[A]) = this(n, f)
        override def addAttrs(a: List[Trace.Attr])(implicit span: Span) = fUnit
        private def apply[A](name: String, f: Span => F[A]) =
          for {
            start <- F point System.nanoTime()
            _     <- F point println(s"Starting $name …")
            a     <- f(())
            end   <- F point System.nanoTime()
            _     <- F point printf("Finished %s in %,3d ns\n", name, end - start)
          } yield a
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
        addAttrs(
          Attr.EndpointName(routeName) ::
          Attr.HttpUri(path.relativeUrl) ::
          attrForHttpReq(req))
        F.map(respond(span)) { res =>
          addAttrs(attrForHttpRes(res))
          res
        }
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
      new Server.Algebra[F] {
        override def delay[A](f: F[A], d: Duration) = orig.delay(f, d)
        override def fork[A](f: F[A])               = orig.fork(f)
        override val clientIP                       = orig.clientIP
        override val sessionId                      = orig.sessionId
        override def now                            = orig.now
        // Customisation:
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