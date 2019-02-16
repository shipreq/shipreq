package bootstrap.liftweb

import com.zaxxer.hikari.metrics.prometheus.PrometheusMetricsTrackerFactory
import japgolly.clearconfig._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import monocle.macros.Lenses
import net.liftweb.common.Logger
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.util.Props.RunModes
import scalaz.syntax.applicative._
import shipreq.base.db.{DbAccess, DbConfig}
import shipreq.base.ops.{JdbcLogging, JdbcMetrics, SqlTracer}
import shipreq.base.util.FxModule._
import shipreq.base.util.{Props => ShipReqProps}
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.app._
import shipreq.webapp.server.lib.Taskman
import shipreq.webapp.server.security.AppSecurityRealm

@Lenses
final case class BootConfig(db: DbConfig, server: ServerConfig, report: ConfigReport)

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
    val (cfg, runMode) = readConfig()
    logger.info(cfg.report.full)
    runMode foreach setRunMode
    logger.info(s"RunMode = ${Props.mode}")

    // Create services
    implicit val serverConfig = cfg.server
    implicit val dbAccess = initDatabase(cfg)
    initShiro()
    configureLift()
    Global.Instance = Global.default

    // Prepare services
    preloadTemplates()
    initOps(Global.Instance)
    initRoutes(Global.Instance)
    initTaskman(Global.Instance)

    // Start services
    initPrometheus(cfg.server.prometheus)
  }

  def readConfig(): (BootConfig, Option[RunModes.Value]) = {

    val cfgRunMode: ConfigDef[Option[RunModes.Value]] =
      ConfigDef.get[String]("shipreq.lift.runMode").mapOption {
        case Some(i) => RunModes.values.iterator.filter(_.toString.toLowerCase ==* i).nextOption().map(Some(_))
        case None    => Some(None)
      }

    val plan = (DbConfig.config |@| ServerConfig.config |@| cfgRunMode).tupled.withReport
      .map { case ((db, svr, runMode), report) =>
        val cfg = BootConfig(db, svr, report)
        (cfg, runMode)
      }

    plan.run(ShipReqProps.sources).unsafeRun().getOrDie()
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
    LiftRules.liftContextRelativePath = WebappConfig.liftPath1

    // Customise URL paths for lift.js
    LiftRules.resourceServerPath = WebappConfig.liftPath2

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

  def initShiro(): Unit =
    AppSecurityRealm.init()

  def initDatabase(cfg: BootConfig): DbAccess = {
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
    if (g.config.initTaskmanOnBoot)
      Taskman.updateCfg(g)
        .retryOnException((n, t) => g.config.initTaskmanRetry(n).map(d => Fx {
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

  def initOps(g: Global): Unit = {
    val m = g.metrics

    val sessionStart: (LiftSession, Req) => Unit =
      (s, _) => m.sessionStart(ServerInterpreter.getSessionId(s)).unsafeRun()

    val sessionEnd: LiftSession => Unit =
      s => m.sessionEnd(ServerInterpreter.getSessionId(s)).unsafeRun()

    LiftSession.afterSessionCreate ::= sessionStart
    LiftSession.onShutdownSession ::= sessionEnd
  }

  def initRoutes(g: Global): Unit = {
    // (Must be done after Global is ready)
    new LiftDispatcher(g).init()
  }

  def initPrometheus(cfg: ServerConfig.Prometheus): Unit =
    if (cfg.enabled) {
      if (cfg.hotspot)
        io.prometheus.client.hotspot.DefaultExports.initialize()
      // See also:
      // - initDatabase()
      // - AppServletFilter
    }
}
