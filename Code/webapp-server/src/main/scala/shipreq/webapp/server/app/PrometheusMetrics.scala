package shipreq.webapp.server.app

import io.prometheus.client.{Counter, Gauge, Histogram, SimpleTimer}
import japgolly.microlibs.stdlib_ext.ParseLong
import java.time.Duration
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import scala.collection.JavaConverters._
import shipreq.base.util.FreeOption
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.webapp.base.user.User
import shipreq.webapp.server.logic.{MetricsLogic, Security, SessionId}
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
    final val Method     = "method"
    final val MsgType    = "msg_type"
    final val Name       = "name"
    final val Ok         = "ok"
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

    val LoginsActive             = new LoginsActive
    val HttpDuration             = new HttpDuration
    val HttpIO                   = new HttpIO
    val HttpSessionsActive       = mkHttpSessionsActive
    val HttpSessionsTotal        = mkHttpSessionsTotal
    val ProjectsActive           = mkProjectsActive
    val SecureEventsTotal        = new SecureEventsTotal
    val WebSocketMessageDuration = new WebSocketMessageDuration
    val WebSocketPushes          = new WebSocketPushes
    val WebSocketIO              = new WebSocketIO

    // The following counters are omitted because they are provided by histogram counts
    //
    // http_requests_total  ← http_response_duration_seconds_count
    // ws_messages_total    ← ws_message_duration_seconds_count

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

    private def mkHttpSessionsActive =
      Gauge.build(prefix + "http_sessions_active", "HTTP sessions currently active").register()

    private def mkHttpSessionsTotal =
      Counter.build(prefix + "http_sessions_total", "Total HTTP sessions created").register()

    final class LoginsActive private[Metrics] {
      private[this] val m =
        Gauge.build(prefix + "logins_active", "Logged-in sessions currently active")
          .labelNames(Label.Unique)
          .register()
      def apply(unique: Boolean) =
        m.labels(yesOrNo(unique))
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

    private def mkProjectsActive =
      Gauge.build(prefix + "projects_active", "Projects currently being served").register()

    final class WebSocketMessageDuration private[Metrics] {
      private[this] val m =
        Histogram.build(prefix + "ws_message_duration_seconds", "Duration of WebSocket requests in seconds")
          .labelNames(Label.Name, Label.MsgType, Label.Ok)
          .buckets(
            0.001, 0.003, 0.005, 0.010, 0.025, 0.050, 0.075,
            0.100, 0.200, 0.300, 0.500, 0.750,
            1, 2, 4, 8)
          .register()
      def apply(ok: Boolean)(implicit name: WebSocketName, msgType: MsgType) =
        m.labels(name.value, msgType.value, yesOrNo(ok))
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
          .labelNames(Label.Dir, Label.Name, Label.Type, Label.MsgType, Label.Ok)
          .register()
      def msg(dir: CommDir, ok: Boolean)(implicit name: WebSocketName, msgType: MsgType) =
        m.labels(dir, name.value, "msg", msgType.value, yesOrNo(ok))
      def push(implicit name: WebSocketName) =
        m.labels(CommDir.Send, name.value, "push", "", yesOrNo(true))
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
          HttpIO(CommDir.Recv).inc(req.getContentLengthLong)

        resp.getHeader("Content-Length") match {
          case null =>
            if (status == 304)
              () // Client's cache validated as non-stale - 0 content in reply
            else if (endpoint == Endpoint.Metrics)
              () // /ops/metrics writes directly to output without calculating size - ignore for now
            else
              logger.warn(s"No Content-Length for request: ${req.getMethod} ${req.getRequestURI} ${resp.getStatus}")
          case ParseLong(len) =>
            if (len > 0)
              HttpIO(CommDir.Send).inc(len)
          case str =>
            logger.warn(s"Unable to parse ${method.value} $path response's Content-Length ($str) as Long.")
        }
      }
    }
  }
}

final class PrometheusMetrics extends MetricsLogic[Fx] {
  import PrometheusMetrics.Data._
  import PrometheusMetrics.Metrics._

  @inline private implicit def durationToSec(d: Duration): Double =
    d.getSeconds.toDouble + d.getNano.toDouble / 1000000000

  private[this] val endpointVar =
    PrometheusMetrics.Unsafe.endpointVar

  private[this] val sessions =
    new ConcurrentHashMap[SessionId, Option[User]]

  private[this] val sessionsActive =
    new AtomicLong(0)

  override def setHttpName(name: String): Fx[Unit] =
    Fx(endpointVar.set(Endpoint.Page(name)))

  override def setServerSideProcName(name: String): Fx[Unit] =
    Fx(endpointVar.set(Endpoint.ServerSideProc(name)))

  // Replace this in future with more variables and prop-test to ensure the larger set of vars don't go out of sync
  private def updateActiveLogins(): Unit = {
    val seen = scala.collection.mutable.LongMap.empty[Unit]
    var total, unique = 0
    sessions.values().asScala.foreach {
      case Some(u) =>
        total += 1
        val id = u.id.value
        if (!seen.contains(id)) {
          seen.update(id, ())
          unique += 1
        }
      case None =>
    }
    LoginsActive(unique = true).set(unique)
    LoginsActive(unique = false).set(total)
  }

  override def sessionStart(id: SessionId): Fx[Unit] =
    Fx {
      sessions.put(id, None)
      HttpSessionsTotal.inc()
      HttpSessionsActive.set(sessionsActive.incrementAndGet())
    }

  override def sessionEnd(id: SessionId): Fx[Unit] =
    Fx {
      val oldUser = sessions.remove(id)
      HttpSessionsActive.set(sessionsActive.decrementAndGet())
      if (oldUser.isDefined)
        updateActiveLogins()
    }

  override def login(sessionId: SessionId, user: User): Fx[Unit] =
    Fx {
      sessions.put(sessionId, Some(user))
      sessions.put(sessionId, Some(user))
      updateActiveLogins()
    }

  override def logout(sessionId: SessionId): Fx[Unit] =
    Fx {
      sessions.computeIfPresent(sessionId, (_, _) => None)
      updateActiveLogins()
    }

  override def securityEvent(event: Security.Event, result: Security.Result): Fx[Unit] =
    Fx(SecureEventsTotal(event, result).inc())

  override def setActiveProjectCount(n: Int): Fx[Unit] =
    Fx(ProjectsActive.set(n))

  private[this] implicit val projectSpa = WebSocketName("project_spa")

  override def projectSpaWebSocketMsg(msgType : String,
                                      bytesIn : Long,
                                      bytesOut: Long,
                                      duration: Duration,
                                      ok      : Boolean): Fx[Unit] =
    Fx {
      implicit val msgTypeT = MsgType(msgType)
      WebSocketMessageDuration(ok).observe(duration)
      WebSocketIO.msg(CommDir.Recv, ok).inc(bytesIn)
      WebSocketIO.msg(CommDir.Send, ok).inc(bytesOut)
    }

  private[this] val projectSpaPushes = WebSocketPushes.apply
  private[this] val projectSpaPushIO = WebSocketIO.push
  override def projectSpaWebSocketPush(bytesOut: Long): Fx[Unit] =
    Fx {
      projectSpaPushes.inc()
      projectSpaPushIO.inc(bytesOut)
    }
}
