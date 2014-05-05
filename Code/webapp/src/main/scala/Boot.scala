package bootstrap.liftweb

import net.liftweb.common.Logger
import net.liftweb.http._
import net.liftweb.util.Props
import net.liftweb.util.Props.RunModes._
import provider.HTTPParam

import shipreq.webapp._
import shipreq.webapp.app.{DI, Defaults, AppSiteMap}
import db.DB
import feature.SessionStats
import lib.Taskman
import security.Oshiro

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends DI {

  LiftRules.configureLogging()

  val packageRoot = "shipreq.webapp"
  lazy val logger = Logger(s"$packageRoot.Boot")

  def boot(): Unit = {
    configureLift()
    preloadTemplates()
    initDatabase()
    initTaskman()
    logImportantSettings()
  }

  def configureLift(): Unit = {

    Oshiro.init()

    // Collect session stats
    LiftSession.afterSessionCreate ::= SessionStats.onSessionCreation _
    LiftSession.onShutdownSession ::= SessionStats.onSessionExpiration _

    // App package path
    LiftRules.addToPackages(packageRoot)

    // Prevent "stable" func names in test, and speed up generation routine
    LiftRules.funcNameGenerator = S.generateFuncName _

    // Register route whitelist
    LiftRules.setSiteMap(AppSiteMap.sitemap)

    // Force requests to be UTF-8
    LiftRules.early.append(_ setCharacterEncoding "UTF-8")

    // Common headers: Remove X-Lift-Version, add X-Frame-Options
    val suppHeaderList: List[HTTPParam] = List(HTTPParam("X-Frame-Options", "DENY"))
    LiftRules.supplimentalHeaders = _ addHeaders suppHeaderList

    // Add support for HAML/Jade template (must be after other LiftRules)
    ScamlJade.init(List("scaml", "html"))
  }

  def initDatabase(): Unit = {
    DB.init()
    Defaults.init()
  }

  def initTaskman(): Unit =
    Props.mode match {
      case Test =>
      case _    => taskman1(_ runS Taskman.updateCfg)
    }

  def preloadTemplates(): Unit = {
    snippet.DynModal
    snippet.Quotes
    snippet.ShareEditConsts
    snippet.project.ProjectHeaderConsts
    snippet.project.ShareListConsts
    snippet.project.UseCaseCrudlConsts
    snippet.uce.Renderer.Templates
  }

  def logImportantSettings(): Unit = {
    import shipreq.webapp.app.AppConfig._
    logger.info(s"Signup allowed: ${AllowRegister()}")
  }
}
