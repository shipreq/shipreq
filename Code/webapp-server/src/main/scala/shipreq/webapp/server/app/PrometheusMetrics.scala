package shipreq.webapp.server.app

import io.prometheus.client.{Counter, Gauge, Histogram, SimpleTimer}
import japgolly.microlibs.stdlib_ext.ParseLong
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import scala.collection.JavaConverters._
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.UserId
import shipreq.webapp.base.user.User
import shipreq.webapp.server.logic.SessionId

object PrometheusMetrics {

  private[PrometheusMetrics] object Metrics {
    import Label._

    val HttpBytes = new HttpBytes
    final class HttpBytes private {
      private[this] val m =
        Counter.build("shipreq_http_bytes_total", "Size of HTTP content in bytes")
          .labelNames(Dir, Method, StatusCode) // TODO endpoint/name
          .register()
      def apply(dir: String, method: String, statusCode: String) =
        m.labels(dir, method, statusCode)
    }

    // Note: Bucket sizes designed around security delay being 120 ms
    // TODO Make 120 the default and only override in tests - also add a note next to the default that refs back here
    val HttpDuration = new HttpDuration
    final class HttpDuration private {
      private[this] val m =
        Histogram.build("shipreq_http_response_duration_seconds", "Duration of HTTP request in seconds")
          .labelNames(Method, StatusCode) // TODO Delay, endpoint/name
          .buckets(0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.125, 0.13, 0.145, 0.17, 0.195, 0.22, 0.3, 0.5, 0.75, 1, 2, 3, 5, 8, 12)
          .register()
      def apply(method: String, statusCode: String) =
        m.labels(method, statusCode)
    }

    val HttpSessionsActive =
      Gauge.build("shipreq_http_sessions_active", "HTTP sessions currently active")
        .register()

    val HttpSessionsTotal =
      Counter.build("shipreq_http_sessions_total", "Total HTTP sessions created")
        .register()

    val LoginsActive = new LoginsActive
    final class LoginsActive private {
      private[this] val m =
        Gauge.build("shipreq_logins_active", "Logged-in sessions currently active")
          .labelNames(Unique)
          .register()
      def apply(unique: Boolean) =
        m.labels(if (unique) LabelValue.Yes else LabelValue.No)
    }

    val SecureRequestsTotal = new SecureRequestsTotal
    final class SecureRequestsTotal private {
      private[this] val m =
        Counter.build("shipreq_secure_requests_total", "Total security-sensitive requests")
          .labelNames(Success, Type)
          .register()
      def apply(success: Boolean) =
        m.labels(if (success) LabelValue.Yes else LabelValue.No)
    }

    val ProjectsActive =
      Gauge.build("shipreq_projects_active", "Projects currently being served")
        .register()
  }

  private[PrometheusMetrics] object Label {
    final val Delay      = "delay"
    final val Dir        = "dir"
    final val Method     = "method"
    final val StatusCode = "status_code"
    final val Success    = "success"
    final val Type       = "type"
    final val Unique     = "unique"
  }

  private[PrometheusMetrics] object LabelValue {
    final val DirIn         = "in"
    final val DirOut        = "out"
    final val StatusCode200 = "200"
    final val StatusCode304 = "304"
    final val No            = "n"
    final val Yes           = "y"
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final class PrometheusMetrics extends HasLogger {
  import PrometheusMetrics.Metrics._
  import PrometheusMetrics.LabelValue._


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
      val method      = req.getMethod
      val statusCode  = status match {
        case 200 => StatusCode200
        case 304 => StatusCode304
        case n   => n.toString
      }

      HttpDuration(method = method, statusCode = statusCode).observe(durationSec)

      if (req.getContentLengthLong > 0)
        HttpBytes(dir = DirIn, method = method, statusCode = statusCode).inc(req.getContentLengthLong)

      resp.getHeader("Content-Length") match {
        case null =>
          if (status != 304)
            log.warn(s"No Content-Length for request: ${req.getMethod} ${req.getServletPath} ${resp.getStatus}")
        case ParseLong(len) =>
          if (len > 0)
            HttpBytes(dir = DirOut, method = method, statusCode = statusCode).inc(len)
        case _ => ()
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

//    val SecureRequestsTotal = new SecureRequestsTotal

//    val ProjectsActive = Gauge.build("shipreq_projects_active", "Projects currently being served")

}
