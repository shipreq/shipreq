package shipreq.webapp.server.app

import io.prometheus.client.exporter.MetricsServlet
import javax.servlet._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import net.liftweb.http.provider.servlet.ServletFilterProvider

final class AppServletFilter extends ServletFilterProvider {

  private class MetricsServlet2 extends MetricsServlet {
    override def service(req: HttpServletRequest, resp: HttpServletResponse): Unit =
      super.service(req, resp)
  }

  override def init(config: FilterConfig): Unit = {
    super.init(config)
    val p = Global.config.prometheus
    if (p.enabled)
      doFilterFn = doFilterFnWithMetrics(p.path)
  }

  override def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain): Unit =
    doFilterFn(req, res, chain)

  private type DoFilterFn = (ServletRequest, ServletResponse, FilterChain) => Unit

  private[this] var doFilterFn: DoFilterFn =
    super.doFilter

  private def doFilterFnWithMetrics(metricsPath: String): DoFilterFn = {
    val metricsExporter = new MetricsServlet2
    val metrics = new PrometheusMetrics

    (req, res, chain) =>
      (req, res) match {
        case (hreq: HttpServletRequest, hres: HttpServletResponse) =>
          metrics.observeHttp(hreq, hres)(
            if (metricsPath == hreq.getServletPath)
              metricsExporter.service(hreq, hres)
            else
              super.doFilter(req, res, chain))
        case _ =>
          super.doFilter(req, res, chain)
      }
  }

}

