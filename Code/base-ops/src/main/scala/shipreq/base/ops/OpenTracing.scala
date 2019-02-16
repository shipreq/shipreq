package shipreq.base.ops

import io.opentracing.Tracer
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.util.FxModule._

object OpenTracing {

  def algebraFx(tracer: Tracer): Trace.Algebra[Fx] =
    new Trace.Algebra[Fx] {

      override type Span = io.opentracing.Span

      private def withNewSpan[A](createSpan: => Span)(f: Span => Fx[A]): Fx[A] =
        Fx {
          val span = createSpan
          unsafeWithSpanActivated(span, true)(f(span).unsafeRun())
        }

      private def withSpanActivated[A](span: Span, closeSpan: Boolean, f: Fx[A]): Fx[A] =
        Fx(unsafeWithSpanActivated(span, closeSpan)(f.unsafeRun()))

      private def unsafeWithSpanActivated[A](span: Span, closeSpan: Boolean)(body: => A): A = {
        val scope = tracer.scopeManager().activate(span, closeSpan)
        try
          body
        catch {
          case t: Throwable =>
            setError(span, t)
            throw t
        }
        finally
          scope.close()
      }

      private def setError(span: Span, err: Throwable): Unit = {
        span.setTag("error", true)
        span.setTag("event", "error")
        span.setTag("error.kind", err.getClass.getName)
        span.setTag("message", err.getMessage)
        span.setTag("stack", err.stackTraceAsString)
      }

      override def newSpan[A](name: String)(f: Span => Fx[A]): Fx[A] =
        withNewSpan(tracer.buildSpan(name).start())(f)

      override def newSubSpan[A](name: String, parent: Span)(f: Span => Fx[A]): Fx[A] =
        withNewSpan(tracer.buildSpan(name).asChildOf(parent).start())(f)

      override def _propagateCtx[A]: Fx[Fx[A] => Fx[A]] =
        Fx((f: Fx[A]) => {
          val span = tracer.activeSpan()
          if (span eq null)
            f
          else
            withSpanActivated(span, false, f)
        })

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

      override def sqlTracer(spanName: String) =
        Some(new SqlTracer {

          /** System.nanoTime() isn't relative to epoch.
            * This offset roughly converts a nanoTime value (in μs) to μs-since-epoch.
            */
          val nanoTimeOffsetUs = {
            val n1         = System.nanoTime()
            val m          = System.currentTimeMillis()
            val n2         = System.nanoTime()
            val n          = n1 + (n2 - n1) / 2
            val nanoTimeUs = n / 1000
            val epochUs    = m * 1000
            epochUs - nanoTimeUs
          }

          override def logExecute(method: String, sql: String, batches: Int,
                                  err: Option[Throwable], startTimeNs: Long, endTimeNs: Long): Unit = {
            val startTimeUs = nanoTimeOffsetUs + startTimeNs / 1000
            val endTimeUs   = nanoTimeOffsetUs + endTimeNs / 1000
            val span        = tracer.buildSpan(spanName).withStartTimestamp(startTimeUs).start()
            span.setTag("jdbc.class", "PreparedStatement")
            span.setTag("jdbc.method", method)
            span.setTag("jdbc.sql", sql)
            span.setTag("jdbc.batches", batches)
            err.foreach(setError(span, _))
            span.finish(endTimeUs)
          }
        })

    }

}
