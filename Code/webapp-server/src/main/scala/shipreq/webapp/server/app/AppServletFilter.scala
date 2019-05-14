package shipreq.webapp.server.app

import io.prometheus.client.exporter.MetricsServlet
import java.util.UUID
import javax.servlet._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import net.liftweb.http.LiftFilter
import org.slf4j.MDC
import shipreq.base.util.log.HasLogger
import shipreq.webapp.base.Urls

/** Servlet entry-point into ShipReq (as specified in web.xml).
  *
  * Delegates to Prometheus' [[MetricsServlet]] to serve metrics.
  * Delegates to LiftFilter otherwise.
  */
final class AppServletFilter extends LiftFilter with HasLogger {

  private type DoFilterFn = (ServletRequest, ServletResponse, FilterChain) => Unit

  private[this] var doFilterFn: DoFilterFn =
    super.doFilter

  override def init(config: FilterConfig): Unit = {

    // Initialise Lift (which in turn boots ShipReq and populates Global)
    super.init(config)
    val g = Global.Instance

    // Initialise Prometheus
    val p = g.config.prometheus
    if (p.enabled) {
      val endpointResolver = Endpoint.resolver(p.path)
      installPrometheus(new PrometheusMetrics.Unsafe(endpointResolver), p.path)
    }

    // Don't handle websockets
    // (Note: This following the Prometheus block above means that PrometheusMetrics doesn't see WebSocket traffic)
    ignore {
      val root = Urls.ProjectSpaWebSocket.Base + "/"
      _.startsWith(root)
    }

    // Initialise logging
    installLogging()
  }

  override def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain): Unit =
    doFilterFn(req, res, chain)

  private def installPrometheus(metrics: PrometheusMetrics.Unsafe, metricsPath: String): Unit = {
    val metricsServlet = new PrometheusMetricsServlet
    val real = doFilterFn
    doFilterFn = (req, res, chain) =>
      if (req.isInstanceOf[HttpServletRequest] && res.isInstanceOf[HttpServletResponse]) {
        val hreq = req.asInstanceOf[HttpServletRequest]
        val hres = res.asInstanceOf[HttpServletResponse]
        metrics.unsafeObserveHttp(hreq, hres)(
          if (metricsPath == hreq.getRequestURI)
            metricsServlet.service(hreq, hres)
          else
            real(req, res, chain))
      } else
        real(req, res, chain)
  }

  /** Don't process matching requests with this filter, leaving them to be handled by the servlet container (Jetty) */
  private def ignore(ignoreUri: String => Boolean): Unit = {
    val real = doFilterFn
    doFilterFn = (req, res, chain) =>
      req match {
        case r: HttpServletRequest if ignoreUri(r.getRequestURI) => chain.doFilter(req, res)
        case _                                                   => real(req, res, chain)
      }
  }

  /** Version of Prometheus' [[MetricsServlet]] that exposes it's service proc */
  private class PrometheusMetricsServlet extends MetricsServlet {
    override def service(req: HttpServletRequest, resp: HttpServletResponse): Unit =
      super.service(req, resp)
  }

  private def installLogging(): Unit = {
    UUID.randomUUID() // Force initialisation
    val real = doFilterFn
    doFilterFn = (req, res, chain) => {

      val startMs = System.currentTimeMillis()
      val txnId = UUID.randomUUID().toString
      MDC.put("txn_id", txnId)
      MDC.put("request_remote_addr", req.getRemoteAddr)
      MDC.put("request_remote_host", req.getRemoteHost)
      val hreq: HttpServletRequest =
        req match {
        case h: HttpServletRequest =>
          h.getRequestURL match {
            case null => ()
            case sb => MDC.put("request_url", sb.toString)
          }
          MDC.put("request_uri", h.getRequestURI)
          MDC.put("request_method", h.getMethod)
          // MDC.put("request_query_string", h.getQueryString)
          MDC.put("request_user_agent", h.getHeader("User-Agent"))
          MDC.put("request_x_forwarded_for", h.getHeader("X-Forwarded-For"))
          h
        case _ =>
          null
      }

      try {
        real(req, res, chain)

        val durMs = System.currentTimeMillis() - startMs
        if ((hreq ne null) && res.isInstanceOf[HttpServletResponse]) {
          val hres = res.asInstanceOf[HttpServletResponse]
          logger.info(s"Served ${hres.getStatus} ${hreq.getRequestURI} in $durMs ms")
        } else
          logger.info(s"Served non-HTTP request in $durMs ms")
      }
      finally
        MDC.clear()
    }
  }

}
