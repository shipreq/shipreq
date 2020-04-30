package shipreq.webapp.server.app

import io.prometheus.client.{Counter, Gauge, Histogram, SimpleTimer}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.stdlib_ext.ParseLong
import java.time.Duration
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import shipreq.base.util.FreeOption
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.webapp.base.event.{Trust, Trusted}
import shipreq.webapp.server.logic.{MetricsLogic, Security}
import shipreq.webapp.server.util.CommDir

object PrometheusMetrics extends HasLogger {

  object Data {
    final case class HttpMethod(value: String) extends AnyVal

    final case class MsgType(value: String) extends AnyVal

    final class StatusCode(val value: String) extends AnyVal
    object StatusCode {
      private[this] val StatusCode101 = "101"
      private[this] val StatusCode200 = "200"
      private[this] val StatusCode302 = "302"
      private[this] val StatusCode304 = "304"
      private[this] val StatusCode403 = "403"
      private[this] val StatusCode404 = "404"
      def apply(value: Int): StatusCode =
        new StatusCode(value match {
          case 200 => StatusCode200
          case 302 => StatusCode302
          case 304 => StatusCode304
          case 403 => StatusCode403
          case 404 => StatusCode404
          case 101 => StatusCode101
          case _   => value.toString
        })
    }

    final case class WebSocketName(value: String) extends AnyVal
  }

  import Data._

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Internal

  private[PrometheusMetrics] object Label {
    final val Delay      = "delay"
    final val Dir        = "dir"
    final val Event      = "event"
    final val Method     = "method"
    final val MsgType    = "msg_type"
    final val Name       = "name"
    final val Op         = "op"
    final val Process    = "process"
    final val Result     = "result"
    final val Step       = "step"
    final val Success    = "success"
    final val StatusCode = "status_code"
    final val Type       = "type"
    final val Unique     = "unique"
  }

  private implicit def commDirLabel(commDir: CommDir): String =
    if (commDir is CommDir.Send) "send" else "recv"

  private def yesOrNo(yes: Boolean): String =
    if (yes) "y" else "n"

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Metrics

  private[PrometheusMetrics] object Metrics {

    val EventApplicationCount    = new EventApplicationCount
    val EventApplicationDuration = new EventApplicationDuration
    val HttpDuration             = new HttpDuration
    val HttpIO                   = new HttpIO
    val OpenWebSockets           = new OpenWebSockets
    val ProjectSpaStepDuration   = new ProjectSpaStepDuration
    val RedisDuration            = new RedisDuration
    val SecureEventsTotal        = new SecureEventsTotal
    val WebSocketEventDuration   = new WebSocketEventDuration
    val WebSocketIO              = new WebSocketIO
    val WebSocketMessageDuration = new WebSocketMessageDuration
    val WebSocketPushes          = new WebSocketPushes
    val WebSocketSessionDuration = new WebSocketSessionDuration

    // The following counters are omitted because they are provided by histogram counts
    //
    // http_requests_total  <- http_response_duration_seconds_count
    // ws_messages_total    <- ws_message_duration_seconds_count

    // -----------------------------------------------------------------------------------------------------------------

    // Don't make this a val because of init order
    private def prefix = "shipreq_webapp_"

    final class HttpDuration private[Metrics] {
      private[this] val m =
        Histogram.build(prefix + "http_response_duration_seconds", "Duration of HTTP request in seconds")
          .labelNames(Label.Method, Label.Name, Label.StatusCode, Label.Type) // TODO Delay
          .buckets(
            0.001, 0.003, 0.005, 0.010, 0.025, 0.050, 0.075, 0.100, // no security delay
            0.121, 0.123, 0.125, 0.130, 0.145, 0.170, 0.195, 0.220, // with 120 ms security delay (see ServerConfig)
            0.300, 0.500, 0.750,
            1, 2, 4, 8)
          .register()
      def apply(implicit method: HttpMethod, endpoint: Endpoint, statusCode: StatusCode) =
        m.labels(method.value, endpoint.name, statusCode.value, endpoint.`type`)
    }

    final class HttpIO private[Metrics] {
      private[this] val m =
        Counter.build(prefix + "http_bytes_total", "Size of HTTP content in bytes")
          .labelNames(Label.Dir, Label.Method, Label.Name, Label.StatusCode, Label.Type)
          .register()
      def apply(dir: CommDir)(implicit method: HttpMethod, endpoint: Endpoint, statusCode: StatusCode) =
        m.labels(dir, method.value, endpoint.name, statusCode.value, endpoint.`type`)
    }

    final class SecureEventsTotal private[Metrics] {
      private[this] val m =
        Counter.build(prefix + "secure_events_total", "Total security-sensitive events that occurred")
          .labelNames(Label.Name, Label.Success)
          .register()

      private[this] val name: Security.Event => String = {
        case Security.Event.Login          => "login"
        case Security.Event.Register1      => "register_1"
        case Security.Event.Register2      => "register_2"
        case Security.Event.ResetPassword1 => "reset_password_1"
        case Security.Event.ResetPassword2 => "reset_password_2"
      }

      def apply(event: Security.Event, result: Security.Result) =
        m.labels(name(event), yesOrNo(result.isSuccess))
    }

    final class OpenWebSockets private[Metrics] {
      private[this] val m =
        Gauge.build(prefix + "ws_open", "WebSockets currently open")
          .labelNames(Label.Name)
          .register()
      def apply(implicit name: WebSocketName) =
        m.labels(name.value)
    }

    final class WebSocketEventDuration private[Metrics] {
      private[this] val m =
        Histogram.build(prefix + "ws_event_duration_seconds", "Duration of WebSocket event handlers (except onMessage)")
          .labelNames(Label.Name, Label.Event, Label.Result)
          .buckets(
            0.001, 0.003, 0.005, 0.010, 0.025, 0.050, 0.075,
            0.100, 0.200, 0.300, 0.500, 0.750,
            1, 2, 4, 8)
          .register()
      def apply(event: String, result: String)(implicit name: WebSocketName) =
        m.labels(name.value, event, result)
    }

    final class WebSocketMessageDuration private[Metrics] {
      private[this] val m =
        Histogram.build(prefix + "ws_message_duration_seconds", "Duration of WebSocket requests in seconds")
          .labelNames(Label.Name, Label.MsgType, Label.Success)
          .buckets(
            0.001, 0.003, 0.005, 0.010, 0.025, 0.050, 0.075,
            0.100, 0.200, 0.300, 0.500, 0.750,
            1, 2, 4, 8)
          .register()
      def apply(success: Boolean)(implicit name: WebSocketName, msgType: MsgType) =
        m.labels(name.value, msgType.value, yesOrNo(success))
    }

    final class WebSocketPushes private[Metrics] {
      private[this] val m =
        Counter.build(prefix + "ws_pushes_total", "Total WebSocket messages pushed from server (excluding responses to request messages)")
          .labelNames(Label.Name)
          .register()
      def apply(implicit name: WebSocketName) =
        m.labels(name.value)
    }

    final class WebSocketIO private[Metrics] {
      private[this] val m =
        Counter.build(prefix + "ws_bytes_total", "WebSocket traffic in bytes")
          .labelNames(Label.Dir, Label.Name, Label.Type, Label.MsgType, Label.Success)
          .register()
      def msg(dir: CommDir, success: Boolean)(implicit name: WebSocketName, msgType: MsgType) =
        m.labels(dir, name.value, "msg", msgType.value, yesOrNo(success))
      def push(implicit name: WebSocketName) =
        m.labels(CommDir.Send, name.value, "push", "", yesOrNo(true))
    }

    final class WebSocketSessionDuration private[Metrics] {
      private[this] val m =
        Histogram.build(prefix + "ws_session_duration_minutes", "Total duration of WebSocket session in minutes")
          .labelNames(Label.Name)
          .buckets(1, 5, 10, 15, 30, 45, 60, 90, 120, 150, 180, 240, 480)
          .register()
      def apply(implicit name: WebSocketName) =
        m.labels(name.value)
    }

    final class ProjectSpaStepDuration private[Metrics] {
      private[this] val m =
        Histogram.build(prefix + "project_step_duration_seconds", "Duration of a Project-SPA step")
          .labelNames(Label.Process, Label.Step)
          .buckets(
            0.001, 0.003, 0.005, 0.010, 0.025, 0.050, 0.075,
            0.100, 0.200, 0.300, 0.500, 0.750,
            1, 2, 4, 8)
          .register()
      def apply(process: String, step: String) =
        m.labels(process, step)
    }

    final class RedisDuration private[Metrics] {
      private[this] val m =
        Histogram.build(prefix + "redis_duration_seconds", "Duration of Redis call (including {,de}serialisation) in seconds")
          .labelNames(Label.Op)
          .buckets(
            0.001, 0.003, 0.005, 0.010, 0.020, 0.030, 0.050, 0.075,
            0.100, 0.150, 0.200, 0.300, 0.500, 0.750,
            1, 2)
          .register()
      def apply(name: String) =
        m.labels(name)
    }

    final class EventApplicationCount private[Metrics] {
      private[this] val m =
        Counter.build(prefix + "eventap_events_total", "Number of events that were applied")
          .labelNames(Label.Method)
          .register()
      private[this] val trusted   = m.labels("trusted")
      private[this] def untrusted = m.labels("untrusted")
      def apply(trust: Trust) = if (trust is Trusted) this.trusted else untrusted
    }

    final class EventApplicationDuration private[Metrics] {
      private[this] val m =
        Histogram.build(prefix + "eventap_duration_seconds", "Time to apply events to a project in seconds")
          .labelNames(Label.Method)
          .buckets(
            0.001, 0.003, 0.005, 0.010, 0.020, 0.030, 0.050, 0.075,
            0.100, 0.150, 0.200, 0.300, 0.500, 0.750,
            1, 2)
          .register()
      private[this] val trusted   = m.labels("trusted")
      private[this] def untrusted = m.labels("untrusted")
      def apply(trust: Trust) = if (trust is Trusted) this.trusted else untrusted
    }
  }

  private[PrometheusMetrics] object Unsafe {
    val endpointVar = new ThreadLocal[Endpoint]()
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  final class Unsafe(endpointResolver: Endpoint.Resolver) {
    import Metrics._

    private def unsafeSecondsSince(startNanos: Long): Double =
      SimpleTimer.elapsedSecondsFromNanos(startNanos, System.nanoTime())

    private[this] val endpointVar =
      Unsafe.endpointVar

    def unsafeObserveHttp(req: HttpServletRequest, resp: HttpServletResponse)(run: => Unit): Unit = {
      val startNanos = System.nanoTime()
      try {
        endpointVar.set(null)
        run
      } finally {
        val durationSec = unsafeSecondsSince(startNanos)
        val path        = req.getRequestURI
        val status      = resp.getStatus

        implicit val method     = HttpMethod(req.getMethod)
        implicit val statusCode = StatusCode(status)

        implicit val endpoint: Endpoint = {
          val provided = FreeOption(endpointVar.get())
          endpointResolver(path, provided).getOrElse {
            logger.warn(s"Unknown endpoint: ${method.value} $path")
            Endpoint.Unknown
          }
        }

        // printf(s"--- %-60s ---> %s\n", path, endpoint.toString)

        HttpDuration.apply.observe(durationSec)

        if (req.getContentLengthLong > 0)
          HttpIO(CommDir.Recv).inc(req.getContentLengthLong.toDouble)

        resp.getHeader("Content-Length") match {
          case null =>
            if (status == 304)
              () // Client's cache validated as non-stale - 0 content in reply
            else if (endpoint == Endpoint.Metrics)
              () // /ops/metrics writes directly to output without calculating size - ignore for now
            else
              logger.info(s"No Content-Length for request: ${req.getMethod} ${req.getRequestURI} ${resp.getStatus}")
          case ParseLong(len) =>
            if (len > 0)
              HttpIO(CommDir.Send).inc(len.toDouble)
          case str =>
            logger.warn(s"Unable to parse ${method.value} $path response's Content-Length ($str) as Long.")
        }
      }
    }
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final class PrometheusMetrics extends MetricsLogic[Fx] {
  import PrometheusMetrics.Data._
  import PrometheusMetrics.Metrics._

  private def time[A](m: Histogram.Child, f: Fx[A]): Fx[A] =
    Fx {
      val t = m.startTimer()
      try {
        val a = f.unsafeRun()
        t.observeDuration()
        a
      } catch {
        case e: Throwable =>
          t.observeDuration()
          throw e
      }
    }

  private[this] val endpointVar =
    PrometheusMetrics.Unsafe.endpointVar

  override def setHttpName(name: String): Fx[Unit] =
    Fx(endpointVar.set(Endpoint.Page(name)))

  override def setServerSideProcName(name: String): Fx[Unit] =
    Fx(endpointVar.set(Endpoint.ServerSideProc(name)))

  override def securityEvent(event: Security.Event, result: Security.Result): Fx[Unit] =
    Fx(SecureEventsTotal(event, result).inc())

  private[this] implicit val projectSpa = WebSocketName("project_spa")
  private[this] val projectSpaOpen    = OpenWebSockets.apply
  private[this] val projectSpaPushes  = WebSocketPushes.apply
  private[this] val projectSpaPushIO  = WebSocketIO.push
  private[this] val projectSpaSession = WebSocketSessionDuration.apply

  override def projectSpaWebSocketMsg(msgType : String,
                                      bytesIn : Long,
                                      bytesOut: Long,
                                      duration: Duration,
                                      success : Boolean): Fx[Unit] =
    Fx {
      implicit val msgTypeT = MsgType(msgType)
      WebSocketMessageDuration(success).observe(duration.asSeconds)
      WebSocketIO.msg(CommDir.Recv, success).inc(bytesIn.toDouble)
      WebSocketIO.msg(CommDir.Send, success).inc(bytesOut.toDouble)
    }

  override def projectSpaWebSocketPush(bytesOut: Long): Fx[Unit] =
    Fx {
      projectSpaPushes.inc()
      projectSpaPushIO.inc(bytesOut.toDouble)
    }

  override def projectSpaWebSocketConnected(dur: Duration, result: String) =
    Fx(WebSocketEventDuration("connect", result).observe(dur.asSeconds))

  override def projectSpaWebSocketOpened(dur: Duration) =
    Fx {
      WebSocketEventDuration("open", "ok").observe(dur.asSeconds)
      projectSpaOpen.inc()
    }

  override def projectSpaWebSocketClosed(dur: Duration, sessionDur: Duration) =
    Fx {
      WebSocketEventDuration("close", "ok").observe(dur.asSeconds)
      projectSpaSession.observe(sessionDur.asMinutes)
      projectSpaOpen.dec()
    }

  override def projectSpaWebSocketStep[A](process: String, step: String)(f: Fx[A]) =
    time(ProjectSpaStepDuration(process, step), f)

  override def redis(opName: String, dur: Duration): Fx[Unit] =
    Fx(RedisDuration(opName).observe(dur.asSeconds))

  override def appliedEvents(eventCount: Int, dur: Duration, trust: Trust) =
    Fx {
      EventApplicationCount(trust).inc(eventCount)
      EventApplicationDuration(trust).observe(dur.asSeconds)
    }
}
