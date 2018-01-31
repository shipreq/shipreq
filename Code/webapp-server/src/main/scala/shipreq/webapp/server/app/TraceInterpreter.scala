package shipreq.webapp.server.app

import japgolly.microlibs.stdlib_ext.StdlibExt._
import javax.servlet.http.HttpServletRequest
import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.LiftResponse
import net.liftweb.http.provider.servlet.HTTPRequestServlet
import shipreq.base.ops.StackdriverTrace
import shipreq.base.util.FxModule._
import shipreq.webapp.server.logic.Trace

object TraceInterpreter {

  type HttpReq = net.liftweb.http.Req
  type HttpRes = Box[LiftResponse]

  type ForLift[F[_]] = Trace.Logic[F, HttpReq, HttpRes]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Implicits {

    implicit val attrForHttpReq: Trace.AttrFor[HttpReq] =
      req =>
        Trace.Attr.HttpMethod(req.requestType.method) ::
          Trace.Attr.HttpUrl(req.request.url) ::
          // TODO asInstanceOf ↓
          Trace.Attr.HttpRequestSize(req.request.asInstanceOf[HTTPRequestServlet].req.asInstanceOf[HttpServletRequest].getContentLengthLong) ::
          req.userAgent.toList.map(Trace.Attr.HttpUserAgent)

    implicit val attrForHttpRes: Trace.AttrFor[HttpRes] = {
      case f: Full[LiftResponse] =>
        val r = f.value.toResponse
        Trace.Attr.httpStatusCode(r.code) ::
          Trace.Attr.HttpResponseSize(r.size) ::
          Nil
      case x: Failure =>
        Trace.Attr.httpStatusCode(500) ::
          x.rootExceptionCause.toList.map(Trace.Attr.Error)
      case Empty =>
        Nil
    }

  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  def stackdriverAlgebra(cfg: StackdriverTrace.Cfg): Trace.Algebra[Fx] =
    new Trace.Algebra[Fx] {
      import com.google.cloud.trace.core._

      private[this] val tracer = cfg.getTracer()

      override type Span = TraceContext

      override def newSpan[A](name: String)(f: Span => Fx[A]): Fx[A] =
        for {
          span <- Fx(tracer.startSpan(name))
          res <- f(span)
        } yield {
          tracer.endSpan(span) // TODO handle exceptions
          res
        }

//      private val getSpanContext: Fx[SpanContext] =
//        Fx(com.google.cloud.trace.Trace.getSpanContextHandler.current)
//
//      override def sub[A](name: String)(f: => Fx[A]) =
//        getSpanContext.flatMap(ctx =>
//          if (ctx.getTraceId == TraceId.invalid)
//            f // Don't trace
//          else
//            generic(name)(f))

      override def newSubSpan[A](name: String, parent: Span)(f: Span => Fx[A]): Fx[A] =
        // TODO Stacktracer doesn't use parent span
        newSpan(name)(f)

      private[this] val attrInterpretter = Trace.Attr.interpret[Labels.Builder, Throwable](
          shipReqUserId    = (l, a) => {l.add(StackdriverTrace.Label.ShipReqUserId, a.value.toString); null},
          shipReqProjectId = (l, a) => {l.add(StackdriverTrace.Label.ShipReqProjectId, a.value.toString); null},
          endpointName     = (l, a) => {l.add(StackdriverTrace.Label.EndpointName, a.value); null},
          httpMethod       = (l, a) => {l.add(StackdriverTrace.Label.HttpMethod, a.value); null},
          httpUri          = (l, a) => {l.add(StackdriverTrace.Label.HttpUri, a.value); null},
          httpUrl          = (l, a) => {l.add(StackdriverTrace.Label.HttpUrl, a.value); null},
          httpUserAgent    = (l, a) => {l.add(StackdriverTrace.Label.HttpUserAgent, a.value); null},
          httpStatusCode   = (l, a) => {l.add(StackdriverTrace.Label.HttpStatusCode, a.str); null},
          httpRequestSize  = (l, a) => {l.add(StackdriverTrace.Label.HttpRequestSize, a.value.toString); null},
          httpResponseSize = (l, a) => {l.add(StackdriverTrace.Label.HttpResponseSize, a.value.toString); null},
          error            = (l, a) => {l.add(StackdriverTrace.Label.ErrorMessage, a.value.getMessage); a.value})

      override def addAttrs(attrs: List[Trace.Attr])(implicit span: Span): Fx[Unit] =
        Fx {
          val labels = Labels.builder()
          for (a <- attrs) {
            val t = attrInterpretter(labels, a)
            if (t ne null)
              tracer.setStackTrace(span, ThrowableStackTraceHelper.createBuilder(t).build)
          }
          tracer.annotateSpan(span, labels.build())
        }
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object KamonAlgebra extends Trace.Algebra[Fx] {
    import _root_.kamon.Kamon

    override type Span = _root_.kamon.trace.Span

    override def newSpan[A](name: String)(f: Span => Fx[A]): Fx[A] =
      Fx {
        val span = Kamon.buildSpan(name).start()
        Kamon.withSpan(span, finishSpan = true) {
          f(span).unsafeRun()
        }
      }

    override def newSubSpan[A](name: String, parent: Span)(f: Span => Fx[A]): Fx[A] =
      Fx {
        val span = Kamon.buildSpan(name).asChildOf(parent).start()
        Kamon.withSpan(span, finishSpan = true) {
          f(span).unsafeRun()
        }
      }

    private[this] val attrInterpretter = Trace.Attr.interpret[Span, Unit](
        shipReqUserId    = (s, a) => s.tag("shipreq.user_id", a.value),
        shipReqProjectId = (s, a) => s.tag("shipreq.project_id", a.value),
        endpointName     = (s, a) => s.tag("endpoint.name", a.value),
        httpMethod       = (s, a) => s.tag("http.method", a.value),
        httpUrl          = (s, a) => s.tag("http.url", a.value),
        httpUri          = (s, a) => s.tag("http.uri", a.value),
        httpUserAgent    = (s, a) => s.tag("http.user_agent", a.value),
        httpStatusCode   = (s, a) => s.tag("http.status_code", a.value),
        httpRequestSize  = (s, a) => s.tag("http.request_size", a.value),
        httpResponseSize = (s, a) => s.tag("http.response_size", a.value),
        error            = (s, a) => s.addError(a.value.getMessage, a.value))

    override def addAttrs(attrs: List[Trace.Attr])(implicit span: Span): Fx[Unit] =
      Fx(attrs.foreach(attrInterpretter(span, _)))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class OpenTracingAlgebra(tracer: io.opentracing.Tracer) extends Trace.Algebra[Fx] {
    override type Span = io.opentracing.Span

    private def withSpan[A](span: Span)(f: Span => Fx[A]): Fx[A] =
      Fx {
        val scope = tracer.scopeManager.activate(span, true)
        try
          f(span).unsafeRun()
        catch {
          case t: Throwable =>
            setError(span, t)
            throw t
        } finally
          scope.close()
      }

    private def setError(span: Span, err: Throwable): Unit = {
      span.setTag("error", true)
      span.setTag("error.kind", err.getClass.getName)
      span.setTag("error.message", err.getMessage)
      span.setTag("error.stacktrace", err.stackTraceAsString)
    }

    override def newSpan[A](name: String)(f: Span => Fx[A]): Fx[A] =
      withSpan(tracer.buildSpan(name).start())(f)

    override def newSubSpan[A](name: String, parent: Span)(f: Span => Fx[A]): Fx[A] =
      withSpan(tracer.buildSpan(name).asChildOf(parent).start())(f)

    private[this] val attrInterpretter = Trace.Attr.interpret[Span, Unit](
        shipReqUserId    = (s, a) => s.setTag("shipreq.user_id", a.value),
        shipReqProjectId = (s, a) => s.setTag("shipreq.project_id", a.value),
        endpointName     = (s, a) => s.setTag("endpoint.name", a.value),
        httpMethod       = (s, a) => s.setTag("http.method", a.value),
        httpUrl          = (s, a) => s.setTag("http.url", a.value),
        httpUri          = (s, a) => s.setTag("http.uri", a.value),
        httpUserAgent    = (s, a) => s.setTag("http.user_agent", a.value),
        httpStatusCode   = (s, a) => s.setTag("http.status_code", a.value),
        httpRequestSize  = (s, a) => s.setTag("http.request_size", a.value),
        httpResponseSize = (s, a) => s.setTag("http.response_size", a.value),
        error            = (s, a) => setError(s, a.value))

    override def addAttrs(attrs: List[Trace.Attr])(implicit span: Span): Fx[Unit] =
      Fx(attrs.foreach(attrInterpretter(span, _)))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def jaegerAlgebra: Trace.Algebra[Fx] = {
    import com.uber.jaeger.Configuration
    import com.uber.jaeger.Tracer
    import com.uber.jaeger.samplers.ConstSampler
    import com.uber.jaeger.reporters.RemoteReporter
    import io.opentracing.References

    System.setProperty(Configuration.JAEGER_AGENT_HOST, "localhost")
    System.setProperty(Configuration.JAEGER_AGENT_PORT, "6831")

    ////    private[this] val config = new Configuration("webapp", Configuration.SamplerConfiguration.fromEnv(), null)
    //    val reporter = new Configuration("webapp", null, null).getTracerBuilder.build().
    //    private[this] val tracer = new Tracer.Builder("webapp", new ConstSampler(true), reporter)

    val config = new Configuration("webapp",
      new Configuration.SamplerConfiguration("const", 1, null),
      Configuration.ReporterConfiguration.fromEnv())
    val tracer = config.getTracer()
    new OpenTracingAlgebra(tracer)
  }
}
