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
import shipreq.webapp.server.logic.SessionId
import shipreq.webapp.server.util.CommDir

object PrometheusMetrics {

  final case class HttpMethod(value: String) extends AnyVal

  final class StatusCode(val value: String) extends AnyVal
  object StatusCode {
    private[this] val StatusCode200 = "200"
    private[this] val StatusCode304 = "304"
    def apply(value: Int): StatusCode =
      new StatusCode(value match {
        case 200 => StatusCode200
        case 304 => StatusCode304
        case _   => value.toString
      })
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Internal

  private[PrometheusMetrics] object Label {
    final val Delay      = "delay"
    final val Dir        = "dir"
    final val Method     = "method"
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

    val HttpBytes = new HttpBytes
    final class HttpBytes private[Metrics] {
      private[this] val m =
        Counter.build(prefix + "http_bytes_total", "Size of HTTP content in bytes")
          .labelNames(Label.Dir, Label.Method, Label.StatusCode) // TODO endpoint/name
          .register()
      def apply(dir: CommDir, method: HttpMethod, statusCode: StatusCode) =
        m.labels(dir, method.value, statusCode.value)
    }

    val HttpDuration = new HttpDuration
    final class HttpDuration private[Metrics] {
      private[this] val m =
        Histogram.build(prefix + "http_response_duration_seconds", "Duration of HTTP request in seconds")
          .labelNames(Label.Method, Label.StatusCode) // TODO Delay, endpoint/name
          .buckets(
            0.005, 0.010, 0.025, 0.050, 0.075, 0.100, // no security delay
            0.125, 0.130, 0.145, 0.170, 0.195, 0.220, // with 120 ms security delay (see ServerConfig)
            0.300, 0.500, 0.750,
            1, 2, 3, 5, 8, 12)
          .register()
      def apply(method: HttpMethod, statusCode: StatusCode) =
        m.labels(method.value, statusCode.value)
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
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final class PrometheusMetrics extends HasLogger {
  import PrometheusMetrics._
  import PrometheusMetrics.Metrics._

  private def unsafeSecondsSince(startNanos: Long): Double =
    SimpleTimer.elapsedSecondsFromNanos(startNanos, System.nanoTime())

  // TODO Comets too
  def unsafeObserveHttp(req: HttpServletRequest, resp: HttpServletResponse)(run: => Unit): Unit = {
    val startNanos = System.nanoTime()
    try
      run
    finally {
      val durationSec = unsafeSecondsSince(startNanos)
      val status      = resp.getStatus
      val method      = HttpMethod(req.getMethod)
      val statusCode  = StatusCode(status)

      HttpDuration(method, statusCode).observe(durationSec)

      if (req.getContentLengthLong > 0)
        HttpBytes(CommDir.Recv, method, statusCode).inc(req.getContentLengthLong)

      resp.getHeader("Content-Length") match {
        case null =>
          if (status != 304)
            log.warn(s"No Content-Length for request: ${req.getMethod} ${req.getServletPath} ${resp.getStatus}")
        case ParseLong(len) =>
          if (len > 0)
            HttpBytes(CommDir.Send, method, statusCode).inc(len)
        case str =>
          log.warn(s"Unable to parse Content-Length '$str' as Long.")
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private[this] val sessions =
    new ConcurrentHashMap[SessionId, Option[User]]

  private[this] val sessionsActive =
    new AtomicLong(0)

  // Replace this in future with more variables and prop-test to ensure the larger set of vars don't go out of sync
  private def updateLoginsActive(): Unit = {
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

  def sessionStart(id: SessionId): Fx[Unit] =
    Fx {
      sessions.put(id, None)
      HttpSessionsTotal.inc()
      HttpSessionsActive.set(sessionsActive.incrementAndGet())
    }

  def sessionEnd(id: SessionId): Fx[Unit] =
    Fx {
      sessions.remove(id)
      HttpSessionsActive.set(sessionsActive.decrementAndGet())
    }

  def trackLogin(sessionId: SessionId, user: User): Fx[Unit] =
    Fx {
      sessions.put(sessionId, Some(user))
      updateLoginsActive()
    }

  def trackLogout(sessionId: SessionId): Fx[Unit] =
    Fx {
      sessions.put(sessionId, None)
      updateLoginsActive()
    }

}
