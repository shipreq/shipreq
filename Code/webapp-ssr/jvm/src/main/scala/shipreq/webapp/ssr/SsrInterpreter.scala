package shipreq.webapp.ssr

import com.typesafe.scalalogging.StrictLogging
import japgolly.clearconfig.ConfigDef
import japgolly.scalagraal._
import japgolly.scalagraal.GraalJs._
import japgolly.scalagraal.GraalBoopickle._
import scala.concurrent.{Await, TimeoutException}
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scalaz.syntax.applicative._
import shipreq.base.util.FxModule._
import shipreq.base.util.Url
import SsrAlgebra.Types._

object SsrInterpreter {

  final case class Config(enabled: Boolean,
                          poolSize: Int,
                          timeoutPublic: FiniteDuration,
                          timeoutProjectSpaLoader: FiniteDuration)
  object Config {
    val default = apply(
      enabled = true,
      poolSize = 2,
      timeoutPublic = 64 millis,
      timeoutProjectSpaLoader = 500 millis)

    def configDef: ConfigDef[Config] =
      ( ConfigDef.getOrUse("enabled",                  default.enabled) |@|
        ConfigDef.getOrUse("pool.size",                default.poolSize) |@|
        ConfigDef.getOrUse("timeout.public",           default.timeoutPublic) |@|
        ConfigDef.getOrUse("timeout.projectSpaLoader", default.timeoutProjectSpaLoader)
      ) (apply)
  }

  def apply(cfg: Config, prometheus: Boolean): SsrInterpreter = {
    val setup = (
      Expr("window = {console: console, navigator: {userAgent:''}}")
       >> Expr.requireFileOnClasspath("webapp-ssr-deps.js")
       >> Expr.requireFileOnClasspath("webapp-ssr.js"))

    val ctx = ContextPool.Builder.fixedThreadPool(cfg.poolSize)
      .fixedContextPerThread()
      .configure { b0 =>
        var b = b0.onContextCreate(setup)
        if (prometheus) b = b.writeMetrics(prometheusWriter)
        b
      }
      .build()

    new SsrInterpreter(ctx, cfg)
  }

  private lazy val prometheusWriter =
    GraalPrometheus.Builder()
      .configureByMetric {
        case ContextMetrics.Metric.Wait =>
          _.bucketsInMs(.01, .1, 1, 2, 4, 8, 16, 32, 48, 100, 1000)

        case ContextMetrics.Metric.Pre
           | ContextMetrics.Metric.Post =>
          _.bucketsInMs(.01, .1, 1, 10, 100)

        case ContextMetrics.Metric.Body
           | ContextMetrics.Metric.Total =>
          _.bucketsInMs(.5, 1, 2, 4, 8, 12, 20, 30, 40, 50, 60, 75, 100, 250, 500, 750, 1000)
      }
      .registerAndBuild()

  def samplePublicInitData: PublicInitData = {
    import shipreq.base.util.Allow
    import shipreq.webapp.client.public.PublicSpaProtocols._
    PublicInitData(
      publicRegistration = Allow,
      loggedInUser = None)
  }

}

final class SsrInterpreter(ctx: ContextPool, cfg: SsrInterpreter.Config) extends SsrAlgebra[Fx] with StrictLogging {

  override def warmup = Fx {
    logger.info("Warming up SSR....")

    val expr = setUrl("https://shipreq.com") >> publicExpr(SsrInterpreter.samplePublicInitData)

    // TODO Warmup config hardcoded
    val warmupFutures =
      Warmup.pool(ctx)(20, expr, s => {
        logger.info(s"SSR $s")
        s.lastEvalAverage(10).millis < 34 || s.totalWarmupTime.seconds > 30
      })

    Await.result(warmupFutures.done, 60 seconds)
    logger.info("Warm up complete.")
  }

  private val setUrl = Expr.compileFnCall1[String](SsrManifest.SetUrl)(identity)

  private def metricLogger[A](name: String): ContextMetrics.Writer = {
    val logHead = s"Rendered $name in "
    ContextMetrics.Writer(s => logger.info(logHead + s.total))
  }

  private def runner[A](name: String, timeout: Duration, expr: A => Expr[String]): A => Fx[Option[Html]] = {
    val mw = metricLogger(name)
    a => run(expr(a), timeout, mw, name)
  }

  private def runnerU[A](name: String, timeout: Duration, expr: A => Expr[String]): (Url.Absolute, A) => Fx[Option[Html]] = {
    (url, a) => {
      val mw = ContextMetrics.Writer(s => logger.info(s"Rendered $name @ ${url.absoluteUrl} in ${s.total}"))
      run(setUrl(url.absoluteUrl) >> expr(a), timeout, mw, name)
    }
  }

  private def run(expr   : Expr[String],
                  timeout: Duration,
                  mw     : ContextMetrics.Writer,
                  name   : String): Fx[Option[Html]] =
    Fx {
      @volatile var cancel = false
      try {
        val maybeCancel = Expr.byName(if (cancel) Expr.unit else expr)
        val expr2       = maybeCancel >> expr
        val future      = ctx.eval(expr2, mw)
        Await.result(future, timeout) match {
          case Right(html) => Some(Html(html))
          case Left(e) =>
            logger.warn(s"ExprError occurred rendering $name", e)
            None
        }
      } catch {
        case _: TimeoutException =>
          cancel = true
          logger.warn(s"Timeout rendering $name")
          None
        case NonFatal(t) =>
          cancel = true
          logger.warn(s"Unhandled exception occurred rendering $name", t)
          None
      }
    }

  private val publicExpr = Expr.compileFnCall1[PublicInitData](SsrManifest.Public)(_.asString)
  override val public = runnerU("public", cfg.timeoutPublic, publicExpr)

  private val projectSpaLoaderExpr = Expr.compileFnCall1[ProjectSpaLoaderData](SsrManifest.ProjectSpaLoader)(_.asString)
  override val projectSpaLoader = runner("projectSpaLoader", cfg.timeoutProjectSpaLoader, projectSpaLoaderExpr)
}
