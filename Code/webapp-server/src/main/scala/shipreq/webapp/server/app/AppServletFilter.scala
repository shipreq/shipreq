package shipreq.webapp.server.app

import io.prometheus.client.exporter.MetricsServlet
import javax.servlet._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import net.liftweb.http.LiftFilter

/** Servlet entry-point into ShipReq (as specified in web.xml).
  *
  * Delegates to Prometheus' [[MetricsServlet]] to serve metrics.
  * Delegates to LiftFilter otherwise.
  */
final class AppServletFilter extends LiftFilter {

  private type DoFilterFn = (ServletRequest, ServletResponse, FilterChain) => Unit

  private[this] var doFilterFn: DoFilterFn =
    super.doFilter

  override def init(config: FilterConfig): Unit = {

    // Initialise Lift (which in turn boots ShipReq and populates Global)
    super.init(config)

    // Initialise Prometheus
    val p = Global.config.prometheus
    if (p.enabled)
      doFilterFn = doFilterFnWithMetrics(p.path)
  }

  override def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain): Unit =
    doFilterFn(req, res, chain)

  private def doFilterFnWithMetrics(metricsPath: String): DoFilterFn = {
    val metricsServlet = new PrometheusMetricsServlet
    val metrics = new PrometheusMetrics

    (req, res, chain) =>
      (req, res) match {
        case (hreq: HttpServletRequest, hres: HttpServletResponse) =>
          metrics.unsafeObserveHttp(hreq, hres)(
            if (metricsPath == hreq.getServletPath)
              metricsServlet.service(hreq, hres)
            else
              super.doFilter(req, res, chain))
        case _ =>
          super.doFilter(req, res, chain)
      }
  }

  /** Version of Prometheus' [[MetricsServlet]] that exposes it's service proc */
  private class PrometheusMetricsServlet extends MetricsServlet {
    override def service(req: HttpServletRequest, resp: HttpServletResponse): Unit =
      super.service(req, resp)
  }

}
