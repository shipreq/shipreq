package bootstrap.liftweb

import com.zaxxer.hikari.metrics.prometheus.PrometheusMetricsTrackerFactory
import japgolly.clearconfig._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.util.concurrent.Executors
import net.liftweb.common.Logger
import net.liftweb.http._
import net.liftweb.util.Props.RunModes
import net.liftweb.util._
import org.redisson.Redisson
import scala.concurrent.{Await, ExecutionContext, Future}
import scalaz.syntax.applicative._
import shipreq.base.db.DbAccessor
import shipreq.base.ops.{JdbcLogging, JdbcMetrics, SqlTracer}
import shipreq.base.util.FxModule._
import shipreq.base.util.{Props => ShipReqProps}
import shipreq.webapp.base.config.WebappConfig
import shipreq.webapp.server.config.{Global, ServerConfig}
import shipreq.webapp.server.http.{HttpStatusHandler, LiftDispatcher}
import shipreq.webapp.server.interpreter._
import shipreq.webapp.server.logic.algebra.TraceAlgebra
import shipreq.webapp.server.logic.config.ServerLogicConfig
import shipreq.webapp.server.logic.impl.MinimalSsrLogic
import shipreq.webapp.server.taskman.Taskman
import shipreq.webapp.ssr.SsrOff

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {

  LiftRules.configureLogging()

  val packageRoot = "shipreq.webapp.server"
  lazy val logger = Logger(s"$packageRoot.Boot")

  installSignalHandlers()

  def boot(): Unit = {
    // Read config
    val (cfg, runMode, cfgReport) = readConfig()
    logger.info(s"Config report:\n${cfgReport.full}")
    runMode foreach setRunMode
    logger.info(s"RunMode = ${Props.mode}")

    cfg.server.traceAlgebraFx.newSpanImpure("Boot") { span =>

      val timeout     = 2.minutes.asFiniteDuration
      val es          = Executors.newFixedThreadPool(3)
      implicit val ec = ExecutionContext.fromExecutorService(es)

      def trace[A](name: String)(a: => A): A =
        cfg.server.traceAlgebraFx.newSpanImpure("Boot:" + name)(_ => a)

      def submit[A](name: String)(f: => Fx[A]) = Future[A] {
        cfg.server.traceAlgebraFx.newSubSpan("Boot:" + name, span)(_ => f).unsafeRun()
      }

      try {

        // Create services
        val dbF         = submit("initDatabase")(Fx(initDatabase(cfg)))
        val redisF      = cfg.redis.map(c => submit("initRedis")(Fx(Redisson.create(c.instance))))
        val ssrF        = submit("initSsr")(initSsr(cfg.server))
        val db          = Await.result(dbF, timeout)
        val redisClient = redisF.map(Await.result(_, timeout))
        val ssr         = Await.result(ssrF, timeout)
        trace("configureLift")(configureLift(cfg))
        Global.Instance = Global.default(db, redisClient, ssr, cfg)

        // Start services
        val f1 = submit("preloadTemplates")(Fx(preloadTemplates()))
        val f2 = submit("initRoutes")(Fx(initRoutes(Global.Instance)))
        val f3 = submit("initPrometheus")(Fx(initPrometheus(cfg.server.prometheus)))

        // Initialise Taskman
        // If the DB is fresh, this will wait until Taskman starts up and creates its DB schema
        // Because that could take a while, do it synchronously instead of async with the timeout
        trace("initTaskman")(initTaskman(Global.Instance))

        // Wait for tasks to complete
        Await.result(Future.sequence(List(f1, f2, f3)), timeout)

      } finally
        es.shutdown()
    }
  }

  def readConfig(): (ServerConfig, Option[RunModes.Value], ConfigReport) = {

    val logVars =
      ConfigDef.getOrUse[String]("LOG_APPENDER", "JSON") <* ConfigDef.external(
        "LOG_LEVEL_ROOT",
        "LOG_LEVEL_SHIPREQ")

    val cfgRunMode: ConfigDef[Option[RunModes.Value]] =
      ConfigDef.get[String]("shipreq.lift.runMode").mapOption {
        case Some(i) => RunModes.values.iterator.filter(_.toString.toLowerCase ==* i).nextOption().map(Some(_))
        case None    => Some(None)
      }

    (ServerConfig.config |@| cfgRunMode <* logVars)
      .tupled
      .withReport
      .map { case ((svr, runMode), report) => (svr, runMode, report) }
      .run(ShipReqProps.sources)
      .unsafeRun()
      .getOrDie()
  }

  def setRunMode(runMode: RunModes.Value): Unit = {
    System.clearProperty("run.mode")
    Props.autoDetectRunModeFn.set(() => runMode)
    if (Props.mode !=* runMode)
      throw new IllegalStateException(s"Run mode (${Props.mode}) ≠ desired run mode ($runMode)")
  }

  def configureLift(cfg: ServerConfig): Unit = {
    val isLocalhost = cfg.server.baseUrl.value.contains("localhost")

    // App package path
    LiftRules.addToPackages(packageRoot)

    // Prevent "stable" func names in test, and speed up generation routine
    LiftRules.funcNameGenerator = () => S.generateFuncName

    // Customise URL paths for built-in resources & AJAX requests
    LiftRules.liftContextRelativePath = WebappConfig.liftCtxPath

    // Disable built-in request logging
    LiftRules.logServiceRequestTiming = false

    // Force requests to be UTF-8
    LiftRules.early.append(_ setCharacterEncoding "UTF-8")

    // Prevent Lift from acting on static resources
    // (Required because Lift otherwise prevents /s/*.xml from being served.)
    LiftRules.liftRequest.prepend {
      case req if req.uri.startsWith("/s/") => false
    }

    // Security policies
    LiftRules.securityRules = () => {
      val nonProd = isLocalhost || Props.devMode || Props.testMode
      import ContentSourceRestriction._
      var base = List(
        Host("https:"), // allow any https content
        Self)
      if (nonProd)
        base ::= Host("http:")
      val js =
        Scheme("data") :: // webtamp inlines small assets
        UnsafeEval     :: // Lift itself needs this
        UnsafeInline   :: // Snippets use this
        base
      val css =
        Scheme("data") :: // webtamp inlines small assets
        UnsafeInline   :: // React styles
        base
      val font =
        Scheme("data") ::
        base
      val img =
        Scheme("data") ::
        base
      val csp = ContentSecurityPolicy(
        defaultSources = base,
        scriptSources  = js,
        styleSources   = css,
        fontSources    = font,
        imageSources   = img)
      SecurityRules(
        https               = Some(HttpsRules.secure).filterNot(_ => nonProd),
        content             = Some(csp),
        frameRestrictions   = Some(FrameRestrictions.Deny),
        enforceInDevMode    = true,
        enforceInOtherModes = true,
        logInDevMode        = true,
        logInOtherModes     = true)
    }

    // Remove X-Lift-Version
    // (This must occur *after* securityRules)
    val supplementalHeaders = LiftRules.supplementalHeaders.default.get().filterNot(_._1 == "X-Lift-Version")
    LiftRules.supplementalHeaders.default.set(() => supplementalHeaders)

    // Handle 404s, 500s, etc
    HttpStatusHandler.init()
  }

  def initDatabase(cfg: ServerConfig): DbAccessor = {
    val dbCfg = cfg.db

    // Hikari
    if (cfg.server.prometheus.enabled && cfg.server.prometheus.hikaricp)
      dbCfg.hikariConfig.setMetricsTrackerFactory(new PrometheusMetricsTrackerFactory())

    // DataSource
    var sqlTracer: SqlTracer = JdbcLogging
    if (cfg.server.prometheus.enabled && cfg.server.prometheus.jdbc)
      sqlTracer = sqlTracer compose JdbcMetrics.sqlTracer("webapp")
    for (t <- cfg.server.traceAlgebraFx.sqlTracer("JDBC"))
      sqlTracer = sqlTracer compose t
    dbCfg.modifyHikariDataSource(sqlTracer.inject)

    val access = DbAccessor.fromCfg(dbCfg).unsafeRun()
    logger.info(s"Connecting to DB: ${access.desc}")
    access.verifyConnectivity.unsafeRun()
    access.migrator.migrate[Fx].unsafeRun()
    access
  }

  def initTaskman(g: Global): Unit =
    if (g.config.server.initTaskmanOnBoot)
      Taskman.updateCfg(g)
        .retryOnException((n, t) => g.config.server.initTaskmanRetry(n).map(d => Fx {
          logger.warn(s"Taskman initialisation error occurred. Retrying...\n${t.getMessage}")
          Thread sleep d.toMillis
        }))
        .unsafeRun()

  def preloadTemplates(): Unit = {
    import shipreq.webapp.server.snippet._
    HomeSpa
    ProjectSpa
    PublicSpa
  }

  def initRoutes(g: Global): Unit = {
    // (Must be done after Global is ready)
    new LiftDispatcher(g).init()
  }

  def initPrometheus(cfg: ServerLogicConfig.Prometheus): Unit =
    if (cfg.enabled) {
      if (cfg.hotspot)
        io.prometheus.client.hotspot.DefaultExports.initialize()
      // See also:
      // - initDatabase()
      // - AppServletFilter
    }

  def initSsr(cfg: ServerLogicConfig) = {
    val ssr =
    if (cfg.ssr.enabled) {
      // Duplicating Global :S
      import TraceInterpreter.Implicits._
      implicit val assetManifest = cfg.assetManifest
      implicit val traceAlgebra  = cfg.traceAlgebraFx
      implicit val trace         = TraceAlgebra.on: TraceInterpreter.ForHttp[Fx]
      implicit val server        = trace.injectServer(ServerInterpreter)
      new MinimalSsrLogic[Fx]
    } else
      new SsrOff[Fx]
    ssr.prepare(cfg.baseUrl, cfg.publicRegistration)
  }

  def installSignalHandlers(): Unit = {
    import sun.misc._

    val handler: SignalHandler = sig => {
      logger.warn(s"Signal received: SIG${sig.getName}")
    }

    val signals = Seq[String](
      "HUP",
      "INT",
      "TERM",
      "XCPU",
      "XFSZ",
//      "KILL", // Signal already used by VM or OS
//      "QUIT", // Signal already used by VM or OS
//      "SEGV", // Signal already used by VM or OS
    )

    for (name <- signals) {
      try
      Signal.handle(new Signal(name), handler)
      catch {
        case t: Throwable =>
          logger.warn(s"Failed to install SIG$name handler. ", t)
      }
    }
  }
}
