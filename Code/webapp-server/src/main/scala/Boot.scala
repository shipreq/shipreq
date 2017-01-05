package bootstrap.liftweb

import japgolly.microlibs.config.{Config, ConfigReport}
import net.liftweb.common.Logger
import net.liftweb.http.{LiftRules, LiftSession, S}
import net.liftweb.util.Props
import net.liftweb.util.Props.RunModes.Test
import scalaz.effect.IO
import scalaz.syntax.applicative._
import shipreq.base.db.{DbAccess, DbConfig}
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.app.{AppSiteMap, DI, ExceptionHandler}
import shipreq.webapp.server.feature.SessionStats
import shipreq.webapp.server.lib.{Taskman, TaskmanImpl}
import shipreq.webapp.server.security.Oshiro

final case class AppConfig(db: DbConfig, server: ServerConfig, report: ConfigReport)

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends DI {

  LiftRules.configureLogging()

  val packageRoot = "shipreq.webapp.server"
  lazy val logger = Logger(s"$packageRoot.Boot")

  def boot(): Unit = {
    val cfg = readConfig()
    logger.info(cfg.report.report)
    initServerConfig(cfg.server)
    initOshiro()
    configureLift()
    preloadTemplates()
    initDatabase(cfg.db)
    initTaskman()
  }

  def readConfig(): AppConfig = {
    val runModeName = Props.mode.toString
    val runMode = shipreq.base.util.RunMode.forName(runModeName) getOrElse sys.error(s"Unrecognised run mode: '$runModeName'")
    val plan = (DbConfig.config |@| ServerConfig.config).tupled.withReport.map { case ((a, b), z) => AppConfig(a, b, z) }
    val cfg = plan.run(runMode.configSources).getOrDie()
    cfg
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

    // Register route whitelist
    LiftRules.setSiteMap(AppSiteMap.sitemap)

    // Force requests to be UTF-8
    LiftRules.early.append(_ setCharacterEncoding "UTF-8")

    // Common headers: Remove X-Lift-Version, add X-Frame-Options
    val suppHeaderList: List[(String, String)] = List("X-Frame-Options" -> "DENY")
    LiftRules.supplementalHeaders.default.set(() => suppHeaderList)

    // Custom error handling
    LiftRules.exceptionHandler.prepend {
      case (_, r, e) => ExceptionHandler.handleServerError(r, e)
    }

    // Add support for HAML/Jade template (must be after other LiftRules)
    ScamlJade.init()
  }

  def initOshiro(): Unit =
    Oshiro.init()

  def initDatabase(dbConfig: DbConfig): Unit = {
    val access = DbAccess.fromCfg(dbConfig)
    logger.info(s"Connecting to DB: ${access.desc}")
    access.verifyConnectivity()
    access.migrator.migrate[IO].unsafePerformIO()
    DI.dbAccess = access
  }

  def initServerConfig(s: ServerConfig): Unit = {
    DI.serverConfig = s
  }

  def initTaskman(): Unit = {
    DI.taskman = new TaskmanImpl(DI.dbAccess.io)
    Props.mode match {
      case Test =>
      case _    => taskman().runAll(Taskman.updateCfg).unsafePerformIO()
    }
  }

  def preloadTemplates(): Unit = {
    import shipreq.webapp.server.snippet._
    DynModal
    Quotes
  }
}
