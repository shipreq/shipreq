package shipreq.webapp.server.app

import japgolly.microlibs.stdlib_ext.StdlibExt._
import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.LiftResponse
import net.liftweb.http.provider.servlet.HTTPRequestServlet
import shipreq.base.ops.StackdriverTrace
import shipreq.base.util.FxModule._
import shipreq.base.util.Identity
import shipreq.webapp.server.logic.Trace

object TraceInterpreter {

  type HttpReq = net.liftweb.http.Req
  type HttpRes = Box[LiftResponse]

  type ForLift[F[_]] = Trace.Logic[F, HttpReq, HttpRes]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Implicits {

    implicit val attrForHttpReq: Trace.AttrFor[HttpReq] =
      req => {
        val r2 = req.request
        var attrs =
          Trace.Attr.HttpMethod(req.requestType.method) ::
            Trace.Attr.HttpUrl(r2.url) ::
            Trace.Attr.HttpRemoteHost(r2.remoteHost) ::
            Trace.Attr.HttpRemotePort(r2.remotePort) ::
            req.userAgent.toList.map(Trace.Attr.HttpUserAgent)
        r2 match {
          case x: HTTPRequestServlet => attrs ::= Trace.Attr.HttpRequestSize(x.req.getContentLengthLong)
          case _                     => ()
        }
        r2.sessionId match {
          case b: Full[String] => attrs ::= Trace.Attr.HttpSessionId(b.value)
          case _               => ()
        }
        attrs
      }

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
/*
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
          endpointName     = (l, a) => {l.add(StackdriverTrace.Label.EndpointName, a.value); null},
          error            = (l, a) => {l.add(StackdriverTrace.Label.ErrorMessage, a.value.getMessage); a.value},
          httpMethod       = (l, a) => {l.add(StackdriverTrace.Label.HttpMethod, a.value); null},
          httpRemoteHost   = (l, a) => {l.add(StackdriverTrace.Label.HttpRemoteHost, a.value); null},
          httpRemotePort   = (l, a) => {l.add(StackdriverTrace.Label.HttpRemotePort, a.value.toString); null},
          httpRequestSize  = (l, a) => {l.add(StackdriverTrace.Label.HttpRequestSize, a.value.toString); null},
          httpResponseSize = (l, a) => {l.add(StackdriverTrace.Label.HttpResponseSize, a.value.toString); null},
          httpSessionId    = (l, a) => {l.add(StackdriverTrace.Label.HttpSessionId, a.value); null},
          httpStatusCode   = (l, a) => {l.add(StackdriverTrace.Label.HttpStatusCode, a.str); null},
          httpUri          = (l, a) => {l.add(StackdriverTrace.Label.HttpUri, a.value); null},
          httpUrl          = (l, a) => {l.add(StackdriverTrace.Label.HttpUrl, a.value); null},
          httpUserAgent    = (l, a) => {l.add(StackdriverTrace.Label.HttpUserAgent, a.value); null},
          shipReqProjectId = (l, a) => {l.add(StackdriverTrace.Label.ShipReqProjectId, a.value.toString); null},
          shipReqUserId    = (l, a) => {l.add(StackdriverTrace.Label.ShipReqUserId, a.value.toString); null})

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
*/

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object KamonAlgebra extends Trace.Algebra[Fx] {
    import kamon.Kamon

    override type Span = kamon.trace.Span
    private[this] val Span = kamon.trace.Span
    private[this] val SpanContextKey = Span.ContextKey

    private def withNewSpan[A](createSpan: => Span)(f: Span => Fx[A]): Fx[A] =
      Fx {
        val span = createSpan
        unsafeWithActiveSpan(span)(f(span).unsafeRun())
      }

    private def withActiveSpan[A](span: Span, f: Fx[A]): Fx[A] =
      Fx(unsafeWithActiveSpan(span)(f.unsafeRun()))

    private def unsafeWithActiveSpan[A](span: Span)(body: => A): A =
      try
        Kamon.withContextKey(SpanContextKey, span) {
          body
        }
      catch {
        case t: Throwable =>
          setError(span, t)
          throw t
      }
      finally
        span.finish()

    private def setError(span: Span, err: Throwable): Unit = {
      span.tag("error", true)
      span.tag("event", "error")
      span.tag("error.kind", err.getClass.getName)
      span.tag("message", err.getMessage)
      span.tag("stack", err.stackTraceAsString)
    }

    override def newSpan[A](name: String)(f: Span => Fx[A]): Fx[A] =
      withNewSpan(Kamon.buildSpan(name).start())(f)

    override def newSubSpan[A](name: String, parent: Span)(f: Span => Fx[A]): Fx[A] =
      withNewSpan(Kamon.buildSpan(name).asChildOf(parent).start())(f)

    override def _propagateCtx[A]: Fx[Fx[A] => Fx[A]] =
      Fx((f: Fx[A]) => {
        val span = Kamon.currentSpan()
        if (span eq null)
          f
        else
          withActiveSpan(span, f)
      })

    private[this] val attrInterpretter = Trace.Attr.interpret[Span, Unit](
        endpointName     = (s, a) => s.tag("endpoint.name", a.value),
        httpMethod       = (s, a) => s.tag("http.method", a.value),
        httpRemoteHost   = (s, a) => s.tag("http.remote_host", a.value),
        httpRemotePort   = (s, a) => s.tag("http.remote_port", a.value),
        httpRequestSize  = (s, a) => s.tag("http.request_size", a.value),
        httpResponseSize = (s, a) => s.tag("http.response_size", a.value),
        httpSessionId    = (s, a) => s.tag("http.session_id", a.value),
        httpStatusCode   = (s, a) => s.tag("http.status_code", a.value),
        httpUri          = (s, a) => s.tag("http.uri", a.value),
        httpUrl          = (s, a) => s.tag("http.url", a.value),
        httpUserAgent    = (s, a) => s.tag("http.user_agent", a.value),
        shipReqProjectId = (s, a) => s.tag("shipreq.project_id", a.value),
        shipReqUserId    = (s, a) => s.tag("shipreq.user_id", a.value),
        error            = (s, a) => setError(s, a.value))

    override def addAttrs(attrs: List[Trace.Attr])(implicit span: Span): Fx[Unit] =
      Fx(attrs.foreach(attrInterpretter(span, _)))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /*
  final class OpenTracingAlgebra(tracer: io.opentracing.Tracer) extends Trace.Algebra[Fx] {
    import com.google.common.collect.ImmutableMap
    import io.opentracing.Scope
    import io.opentracing.log.Fields
    import io.opentracing.tag.Tags

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
      Tags.ERROR.set(span, true)
      span.log(new ImmutableMap.Builder()
        .put(Fields.EVENT, "error")
        .put(Fields.ERROR_KIND, err.getClass.getName)
        .put(Fields.MESSAGE, err.getMessage)
        .put(Fields.STACK, err.stackTraceAsString)
        .build())
    }

    override def newSpan[A](name: String)(f: Span => Fx[A]): Fx[A] =
      withSpan(tracer.buildSpan(name).start())(f)

    override def newSubSpan[A](name: String, parent: Span)(f: Span => Fx[A]): Fx[A] =
      withSpan(tracer.buildSpan(name).asChildOf(parent).start())(f)

    private[this] val attrInterpretter = Trace.Attr.interpret[Span, Unit](
        endpointName     = (s, a) => s.setTag("endpoint.name", a.value),
        httpMethod       = (s, a) => s.setTag("http.method", a.value),
        httpRemoteHost   = (s, a) => s.setTag("http.remote_host", a.value),
        httpRemotePort   = (s, a) => s.setTag("http.remote_port", a.value),
        httpRequestSize  = (s, a) => s.setTag("http.request_size", a.value),
        httpResponseSize = (s, a) => s.setTag("http.response_size", a.value),
        httpSessionId    = (s, a) => s.setTag("http.session_id", a.value),
        httpStatusCode   = (s, a) => s.setTag("http.status_code", a.value),
        httpUri          = (s, a) => s.setTag("http.uri", a.value),
        httpUrl          = (s, a) => s.setTag("http.url", a.value),
        httpUserAgent    = (s, a) => s.setTag("http.user_agent", a.value),
        shipReqProjectId = (s, a) => s.setTag("shipreq.project_id", a.value),
        shipReqUserId    = (s, a) => s.setTag("shipreq.user_id", a.value),
        error            = (s, a) => setError(s, a.value))

    override def addAttrs(attrs: List[Trace.Attr])(implicit span: Span): Fx[Unit] =
      Fx(attrs.foreach(attrInterpretter(span, _)))

    private[this] val closeScope = (s: Scope) => Fx(s.close())

    override protected def _propagateCtx[A]: Fx[Fx[A] => Fx[A]] =
      Fx {
        val span = tracer.activeSpan()
        if (span eq null)
          Identity.apply
        else {
//          val reinstate = Fx(tracer.scopeManager().activate(span, false))
          val reinstate = Fx {
            val s = tracer.asChildOf(span).start()
            tracer.scopeManager().activate(s, true)
          }
          (f: Fx[A]) => reinstate.bracket(close, _ => f)
        }
      }
  }
  */

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /*
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
  */
}
