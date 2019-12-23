package shipreq.webapp.server.app

import io.prometheus.client.exporter.MetricsServlet
import java.time.Duration
import java.util.UUID
import javax.servlet._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import net.liftweb.http.LiftFilter
import org.slf4j.MDC
import scala.annotation.tailrec
import shipreq.base.util.FxModule._
import shipreq.base.util.log.{HasLogger, WebappLogFields}
import shipreq.webapp.base.Urls
import shipreq.webapp.server.logic.Security
import shipreq.webapp.server.logic.dispatch.Cookie

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
    val p = g.config.server.prometheus
    if (p.enabled) {
      val endpointResolver = Endpoint.resolver(p.path)
      installPrometheus(new PrometheusMetrics.Unsafe(endpointResolver), p.path, p.bearerToken)
    }

    // Don't handle websockets
    // (Note: This following the Prometheus block above means that PrometheusMetrics doesn't see WebSocket traffic)
    ignore {
      val root = Urls.ProjectSpaWebSocket.Base + "/"
      _.startsWith(root)
    }

    // Initialise logging
    installLogging(g.security)
  }

  override def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain): Unit =
    doFilterFn(req, res, chain)

  private def installPrometheus(metrics            : PrometheusMetrics.Unsafe,
                                metricsPath        : String,
                                expectedBearerToken: Option[String]): Unit = {
    val metricsServlet = new PrometheusMetricsServlet(expectedBearerToken)
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

  private def installLogging(security: Security.Algebra[Fx]): Unit = {
    UUID.randomUUID() // Force initialisation
    val real = doFilterFn
    doFilterFn = (req, res, chain) => {

      val startMs = System.currentTimeMillis()
      val requestId = UUID.randomUUID()

      try {
        WebappLogFields.request.id.mdcUnsafePut(requestId)

        // MDC.put("request_remote_addr", req.getRemoteAddr) <-- no point, it's always the ALB
        // MDC.put("request_remote_host", req.getRemoteHost) <-- no point, it's always the ALB

        val hreq: HttpServletRequest =
          req match {
            case h: HttpServletRequest =>

              WebappLogFields.request.uri          .mdcUnsafePut(h.getRequestURI)
              WebappLogFields.request.method       .mdcUnsafePut(h.getMethod)
              WebappLogFields.request.userAgent    .mdcUnsafePut(h.getHeader("User-Agent"))
              WebappLogFields.request.xForwardedFor.mdcUnsafePut(h.getHeader("X-Forwarded-For"))

              h.getRequestURL match {
                case null => ()
                case sb   => WebappLogFields.request.url.mdcUnsafePut(sb.toString)
              }

              // This results in double parsing of JWTs (i.e. once here and then again in server-logic) but...
              // 1. I don't think it can be avoided. We can't pass the result around from here and thread-locals are
              //    out of the question because it's (shit and) too dangerous. MDC is thread-local too but there's
              //    propagation support in the relevant places, and getting it wrong doesn't affect users.
              // 2. It's super fast at around 6us.
              val cookies = h.getCookies
              if (cookies ne null) {
                val lookup: Cookie.LookupFn = name => {
                  // Below is equivalent to: cookies.find(_.getName == name.value).map(_.getValue)
                  @tailrec def go(i: Int): Option[String] =
                    if (i == cookies.length)
                      None
                    else {
                      val c = cookies(i)
                      if (c.getName == name.value)
                        Some(c.getValue)
                      else
                        go(i + 1)
                    }
                  go(0)
                }
                security.sessionRestore(lookup).unsafeRun() match {
                  case Security.SessionRestoreResult.Success(session) =>
                    for (id <- session.sessionId)
                      WebappLogFields.jwt.sessionId.mdcUnsafePut(id.value)
                    for (user <- session.authenticatedUser) {
                      WebappLogFields.jwt.username.mdcUnsafePut(user.username.value)
                      WebappLogFields.jwt.userId.mdcUnsafePut(user.id.value)
                    }
                  case Security.SessionRestoreResult.None =>
                    ()
                  case Security.SessionRestoreResult.Expired(session) =>
                    for (id <- session.sessionId)
                      WebappLogFields.jwt.sessionId.mdcUnsafePut(id.value)
                }
              }

              h
            case _ =>
              null
          }

        real(req, res, chain)

        // -------------------------------------------- Success --------------------------------------------

        val durMs  = System.currentTimeMillis() - startMs
        val dur    = Duration.ofMillis(durMs)
        val durLog = WebappLogFields.response.durMs(dur)

        if ((hreq ne null) && res.isInstanceOf[HttpServletResponse]) {
          val hres = res.asInstanceOf[HttpServletResponse]
          val code = hres.getStatus
          logger.info(s"Served $code ${hreq.getRequestURI} in $durMs ms",
            durLog,
            WebappLogFields.response.code(code))

        } else
          logger.info(s"Served non-HTTP request in $durMs ms", durLog)

      } catch {
        case err: Throwable =>

          // -------------------------------------------- Failure --------------------------------------------

          val durMs  = System.currentTimeMillis() - startMs
          val dur    = Duration.ofMillis(durMs)
          val durLog = WebappLogFields.response.durMs(dur)

          logger.error(s"Error serving request: {}", err, durLog)
          throw err

      } finally
        MDC.clear()
    }
  }

}
