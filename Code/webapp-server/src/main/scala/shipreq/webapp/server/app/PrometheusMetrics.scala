package shipreq.webapp.server.app

import io.prometheus.client.{Counter, Gauge, Histogram, SimpleTimer}
import japgolly.microlibs.stdlib_ext.ParseLong
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import scala.collection.JavaConverters._
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.webapp.base.user.User
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.logic.{MetricsLogic, SessionId}
import shipreq.webapp.server.util.CommDir

object PrometheusMetrics extends HasLogger {

  final case class HttpMethod(value: String) extends AnyVal

  final class StatusCode(val value: String) extends AnyVal
  object StatusCode {
    private[this] val StatusCode200 = "200"
    private[this] val StatusCode302 = "302"
    private[this] val StatusCode304 = "304"
    private[this] val StatusCode404 = "404"
    def apply(value: Int): StatusCode =
      new StatusCode(value match {
        case 200 => StatusCode200
        case 302 => StatusCode302
        case 304 => StatusCode304
        case 404 => StatusCode404
        case _   => value.toString
      })
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Internal

  private[PrometheusMetrics] object Label {
    final val Delay      = "delay"
    final val Dir        = "dir"
    final val Method     = "method"
    final val Name       = "name"
    final val StatusCode = "status_code"
    final val Unique     = "unique"
  }

  private implicit def commDirLabel(commDir: CommDir): String =
    if (commDir is CommDir.Send) "send" else "recv"

  private def yesOrNo(yes: Boolean): String =
    if (yes) "y" else "n"

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Metrics

  private[PrometheusMetrics] object Metrics {
    private val prefix = "shipreq_webapp_"

    val HttpRequests = new HttpRequests
    final class HttpRequests private[Metrics] {
      private[this] val m =
        Counter.build(prefix + "http_requests_total", "Total HTTP requests received")
          .labelNames(Label.Method, Label.Name, Label.StatusCode)
          .register()
      def apply(implicit method: HttpMethod, endpoint: Endpoint, statusCode: StatusCode) =
        m.labels(method.value, endpoint.value, statusCode.value)
    }

    val HttpDuration = new HttpDuration
    final class HttpDuration private[Metrics] {
      private[this] val m =
        Histogram.build(prefix + "http_response_duration_seconds", "Duration of HTTP request in seconds")
          .labelNames(Label.Method, Label.Name, Label.StatusCode) // TODO Delay
          .buckets(
            0.005, 0.010, 0.025, 0.050, 0.075, 0.100, // no security delay
            0.125, 0.130, 0.145, 0.170, 0.195, 0.220, // with 120 ms security delay (see ServerConfig)
            0.300, 0.500, 0.750,
            1, 2, 3, 5, 8, 12)
          .register()
      def apply(implicit method: HttpMethod, endpoint: Endpoint, statusCode: StatusCode) =
        m.labels(method.value, endpoint.value, statusCode.value)
    }

    val HttpIO = new HttpIO
    final class HttpIO private[Metrics] {
      private[this] val m =
        Counter.build(prefix + "http_bytes_total", "Size of HTTP content in bytes")
          .labelNames(Label.Dir, Label.Method, Label.Name, Label.StatusCode)
          .register()
      def apply(dir: CommDir)(implicit method: HttpMethod, endpoint: Endpoint, statusCode: StatusCode) =
        m.labels(dir, method.value, endpoint.value, statusCode.value)
    }

    val HttpSessionsActive =
      Gauge.build(prefix + "http_sessions_active", "HTTP sessions currently active")
        .register()

    val HttpSessionsTotal =
      Counter.build(prefix + "http_sessions_total", "Total HTTP sessions created")
        .register()

    val LoginsActive = new LoginsActive
    final class LoginsActive private[Metrics] {
      private[this] val m =
        Gauge.build(prefix + "logins_active", "Logged-in sessions currently active")
          .labelNames(Label.Unique)
          .register()
      def apply(unique: Boolean) =
        m.labels(yesOrNo(unique))
    }

//    val SecureRequestsTotal = new SecureRequestsTotal
//    final class SecureRequestsTotal private[Metrics] {
//      private[this] val m =
//        Counter.build(prefix + "secure_requests_total", "Total security-sensitive requests processed")
//          .labelNames(Outcome, Type)
//          .register()
//    }

    val ProjectsActive =
      Gauge.build(prefix + "projects_active", "Projects currently being served")
        .register()
  }

  private[PrometheusMetrics] object Unsafe {
    val endpointVar = new ThreadLocal[String]()
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  final class Unsafe(metricsCfg: ServerConfig.Prometheus) {
    import Metrics._

    private def unsafeSecondsSince(startNanos: Long): Double =
      SimpleTimer.elapsedSecondsFromNanos(startNanos, System.nanoTime())

    private[this] val getEndpoint =
      Endpoint.resolver(metricsPath = metricsCfg.path)

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

        implicit val endpoint: Endpoint =
          endpointVar.get() match {
            case null =>
              getEndpoint(path).getOrElse {
                log.warn(s"Unknown endpoint: ${method.value} $path")
                Endpoint.Unknown
              }
            case s =>
              Endpoint.Specified(s)
          }

        // printf(s"--- %-60s ---> %s\n", path, endpoint.toString)

        HttpRequests.apply.inc()

        // Duration in the context of a comet (which is a long-poll) is how long after the user loads the page until
        // the server decides it needs to push something to them.
        if (endpoint != Endpoint.Comet) {
          HttpDuration.apply.observe(durationSec)
        }

        if (req.getContentLengthLong > 0)
          HttpIO(CommDir.Recv).inc(req.getContentLengthLong)

        resp.getHeader("Content-Length") match {
          case null =>
            if (status == 304)
              () // Client's cache validated as non-stale - 0 content in reply
            else if (endpoint == Endpoint.Metrics)
              () // /ops/metrics writes directly to output without calculating size - ignore for now
            else
              log.warn(s"No Content-Length for request: ${req.getMethod} ${req.getRequestURI} ${resp.getStatus}")
          case ParseLong(len) =>
            if (len > 0)
              HttpIO(CommDir.Send).inc(len)
          case str =>
            log.warn(s"Unable to parse ${method.value} $path response's Content-Length ($str) as Long.")
        }
      }
    }
  }
}

final class PrometheusMetrics extends MetricsLogic[Fx] {
  import PrometheusMetrics.Metrics._

  private[this] val endpointVar =
    PrometheusMetrics.Unsafe.endpointVar

  private[this] val sessions =
    new ConcurrentHashMap[SessionId, Option[User]]

  private[this] val sessionsActive =
    new AtomicLong(0)

  override def setHttpName(name: String): Fx[Unit] =
    Fx(endpointVar.set(name))

  override def setServerSideProcName(name: String): Fx[Unit] =
    Fx(endpointVar.set(name))

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
      updateActiveLogins()
    }

  override def logout(sessionId: SessionId): Fx[Unit] =
    Fx {
      sessions.computeIfPresent(sessionId, (_, _) => None)
      updateActiveLogins()
    }

}
