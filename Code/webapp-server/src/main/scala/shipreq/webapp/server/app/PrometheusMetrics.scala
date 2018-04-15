package shipreq.webapp.server.app

import io.prometheus.client.{Histogram, SimpleTimer}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import shipreq.base.util.log.HasLogger

final class PrometheusMetrics extends HasLogger {

  // TODO endpoint/name

  private[this] val duration =
    Histogram.build("http_request_duration_seconds", "Duration of HTTP request in seconds")
      .labelNames("method", "status_code")
      .register()

  private[this] val requestSize =
    Histogram.build("http_request_bytes", "Size of HTTP request content in bytes")
      .labelNames("method", "status_code")
      .register()

  private[this] val responseSize =
    Histogram.build("http_response_bytes", "Size of HTTP response content in bytes")
      .labelNames("method", "status_code")
      .register()

  private[this] val `200` = "200"
  private[this] val `304` = "304"

  def observeHttp(req: HttpServletRequest, resp: HttpServletResponse)(run: => Unit): Unit = {
    val startNanos = System.nanoTime()
    try
      run
    finally {
      val endNanos = System.nanoTime()
      val durationSec = SimpleTimer.elapsedSecondsFromNanos(startNanos, endNanos)
      val status = resp.getStatus
      val labels = new Array[String](2)
      labels(0) = req.getMethod
      labels(1) = status match {
        case 200 => `200`
        case 304 => `304`
        case n => n.toString
      }
      duration.labels(labels: _*).observe(durationSec)
      if (req.getContentLengthLong >= 0)
        requestSize.labels(labels: _*).observe(req.getContentLengthLong)
      resp.getHeader("Content-Length") match {
        case null =>
          if (status != 304)
          log.warn(s"No Content-Length for request: ${req.getMethod} ${req.getServletPath} ${resp.getStatus}")
        case x => responseSize.labels(labels: _*).observe(x.toDouble)
      }
    }
  }

}
