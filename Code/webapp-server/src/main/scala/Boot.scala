package bootstrap.liftweb

import japgolly.microlibs.config.{Config, ConfigParser, ConfigReport}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import monocle.macros.Lenses
import net.liftweb.common.Logger
import net.liftweb.http._
import net.liftweb.util.Props
import net.liftweb.util.Props.RunModes
import scalaz.effect.IO
import scalaz.syntax.applicative._
import shipreq.base.db.{DbAccess, DbConfig}
import shipreq.base.util.{Props => ShipReqProps}
import shipreq.base.util.effect.IoUtils._
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.app._
import shipreq.webapp.server.feature.SessionStats
import shipreq.webapp.server.lib.Taskman
import shipreq.webapp.server.security.AppSecurityRealm

@Lenses
final case class AppConfig(db: DbConfig, server: ServerConfig, report: ConfigReport)

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
    val (appConfig, runMode) = readConfig()
    logger.info(appConfig.report.report)
    runMode foreach setRunMode

    // Create services
    implicit val serverConfig = appConfig.server
    implicit val dbAccess = initDatabase(appConfig.db)
    initShiro()
    configureLift()
    Global.Instance = Global.default

    // Prepare services
    preloadTemplates()
    initRoutes(Global.Instance)
    initTaskman(Global.Instance)
  }

  def readConfig(): (AppConfig, Option[RunModes.Value]) = {
    import ConfigParser.Implicits.Defaults._

    val cfgRunMode: Config[Option[RunModes.Value]] =
      Config.get[String]("shipreq.lift.runMode").mapOption {
        case Some(i) => RunModes.values.iterator.filter(_.toString.toLowerCase ==* i).nextOption().map(Some(_))
        case None    => Some(None)
      }

    val plan = (DbConfig.config |@| ServerConfig.config |@| cfgRunMode).tupled.withReport
      .map { case ((a, b, r), z) => (AppConfig(a, b, z), r) }

    plan.run(ShipReqProps.sources).unsafePerformIO().getOrDie()
  }

  def setRunMode(runMode: RunModes.Value): Unit = {
    System.clearProperty("run.mode")
    Props.autoDetectRunModeFn.set(() => runMode)
    assert(Props.mode ==* runMode)
  }

  def configureLift(): Unit = {

    // Collect session stats
    LiftSession.afterSessionCreate ::= SessionStats.onSessionCreation _
    LiftSession.onShutdownSession ::= SessionStats.onSessionExpiration _

    // App package path
    LiftRules.addToPackages(packageRoot)

    // Prevent "stable" func names in test, and speed up generation routine
    LiftRules.funcNameGenerator = S.generateFuncName _

    // Customise URL paths for built-in resources & AJAX requests
    LiftRules.liftContextRelativePath = WebappConfig.liftPath

    // Force requests to be UTF-8
    LiftRules.early.append(_ setCharacterEncoding "UTF-8")

    // Security policies
    LiftRules.securityRules = () => {
      import ContentSourceRestriction._
      val base = List(
        Host("https:"), // allow any https content
        Scheme("data"), // webtamp inlines small assets
        Self)
      val js =
        UnsafeEval ::   // Lift itself needs this
        UnsafeInline :: // old-school form snippets (like Login) use this
        base
      val css =
        UnsafeInline :: // For React styles
        base
      val csp = ContentSecurityPolicy(
        scriptSources  = js,
        styleSources  = css,
        imageSources   = Nil,
        defaultSources = base)
      SecurityRules(
        https               = Some(HttpsRules.secure).filterNot(_ => Props.devMode || Props.testMode),
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

    // Custom error handling
    LiftRules.exceptionHandler.prepend {
      case (_, r, e) => ExceptionHandler.handleServerError(r, e)
    }
  }

  def initShiro(): Unit =
    AppSecurityRealm.init()

  def initDatabase(dbConfig: DbConfig): DbAccess = {
    val access = DbAccess.fromCfg(dbConfig).unsafePerformIO()
    logger.info(s"Connecting to DB: ${access.desc}")
    access.verifyConnectivity()
    access.migrator.migrate[IO].unsafePerformIO()
    access
  }

  def initTaskman(g: Global): Unit =
    if (g.config.initTaskmanOnBoot)
      Taskman.updateCfg(g)
        .retryOnException((n, t) => g.config.initTaskmanRetry(n).map(d => IO {
          logger.warn(s"Taskman initialisation error occurred. Retrying...\n${t.getMessage}")
          Thread sleep d.toMillis
        }))
        .unsafePerformIO()

  def preloadTemplates(): Unit = {
    import shipreq.webapp.server.snippet._
    HomeSpa
    ProjectSpa
    PublicSpa
  }

  def initRoutes(g: Global): Unit = {
    // (Must be done after Global is ready)
    LiftRules.dispatch.append(new LiftDispatcher(g).dispatchPF)
  }
}
