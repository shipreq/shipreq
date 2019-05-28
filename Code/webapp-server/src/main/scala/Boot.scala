package bootstrap.liftweb

import com.zaxxer.hikari.metrics.prometheus.PrometheusMetricsTrackerFactory
import japgolly.clearconfig._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import net.liftweb.common.Logger
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.util.Props.RunModes
import org.redisson.Redisson
import scalaz.syntax.applicative._
import shipreq.base.db.DbAccess
import shipreq.base.ops.{JdbcLogging, JdbcMetrics, SqlTracer}
import shipreq.base.util.FxModule._
import shipreq.base.util.{Props => ShipReqProps}
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.server.ServerLogicConfig
import shipreq.webapp.server.app._
import shipreq.webapp.server.lib.Taskman

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {

  LiftRules.configureLogging()

  val packageRoot = "shipreq.webapp.server"
  lazy val logger = Logger(s"$packageRoot.Boot")

  def boot(): Unit = {
    // Read config
    val (cfg, runMode, cfgReport) = readConfig()
    logger.info(cfgReport.full)
    runMode foreach setRunMode
    logger.info(s"RunMode = ${Props.mode}")

    import cfg.server.traceAlgebraFx.{newSpanImpure => trace}
    trace("Boot") { _ =>

      // Create services
      val dbAccess = trace("initDatabase")(_ => initDatabase(cfg))
      val redisClient = cfg.redis.map(c => trace("initRedis")(_ => Redisson.create(c.instance)))
      trace("configureLift")(_ => configureLift())
      Global.Instance = trace("Global")(_ => Global.default(dbAccess, redisClient, cfg))

      // Prepare services
      trace("preloadTemplates")(_ => preloadTemplates())
      trace("initRoutes")(_ => initRoutes(Global.Instance))
      trace("initTaskman")(_ => initTaskman(Global.Instance))

      // Start services
      trace("initPrometheus")(_ => initPrometheus(cfg.server.prometheus))
    }

    // Warmup
    Global.Instance.ssr.warmup.unsafeRun()
  }

  def readConfig(): (ServerConfig, Option[RunModes.Value], ConfigReport) = {

    val cfgRunMode: ConfigDef[Option[RunModes.Value]] =
      ConfigDef.get[String]("shipreq.lift.runMode").mapOption {
        case Some(i) => RunModes.values.iterator.filter(_.toString.toLowerCase ==* i).nextOption().map(Some(_))
        case None    => Some(None)
      }

    (ServerConfig.config |@| cfgRunMode)
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

  def configureLift(): Unit = {

    // App package path
    LiftRules.addToPackages(packageRoot)

    // Prevent "stable" func names in test, and speed up generation routine
    LiftRules.funcNameGenerator = S.generateFuncName _

    // Customise URL paths for built-in resources & AJAX requests
    LiftRules.liftContextRelativePath = WebappConfig.liftCtxPath

    // Disable built-in request logging
    LiftRules.logServiceRequestTiming = false

    // Force requests to be UTF-8
    LiftRules.early.append(_ setCharacterEncoding "UTF-8")

    // Security policies
    LiftRules.securityRules = () => {
      val nonProd = Props.devMode || Props.testMode
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

  def initDatabase(cfg: ServerConfig): DbAccess = {
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

    val access = DbAccess.fromCfg(dbCfg).unsafeRun()
    logger.info(s"Connecting to DB: ${access.desc}")
    access.verifyConnectivity()
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
}
