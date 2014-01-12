package bootstrap.liftweb

import javax.mail.{Authenticator, PasswordAuthentication}
import net.liftweb.common.Logger
import net.liftweb.http._
import net.liftweb.util.{Props, Mailer}
import provider.HTTPParam

import com.beardedlogic.shipreq._
import app.{Defaults, AppSiteMap}
import db.DB
import feature.SessionStats
import security.Oshiro

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {

  LiftRules.configureLogging()

  val packageRoot = "com.beardedlogic.shipreq"
  lazy val logger = Logger(s"$packageRoot.Boot")

  def boot(): Unit = {
    configureLift()
    preloadTemplates()
    initDatabase()
    logImportantSettings()
  }

  def configureLift(): Unit = {

    Oshiro.init()

    initMailer()

    // Collect session stats
    LiftSession.afterSessionCreate ::= SessionStats.onSessionCreation _
    LiftSession.onShutdownSession ::= SessionStats.onSessionExpiration _

    // App package path
    LiftRules.addToPackages(packageRoot)

    // Register route whitelist
    LiftRules.setSiteMap(AppSiteMap.sitemap)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))

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

  def initMailer(): Unit = {
    Mailer.authenticator = for {
      user <- Props.get("mail.user")
      pass <- Props.get("mail.password")
    } yield new Authenticator {
        override def getPasswordAuthentication = new PasswordAuthentication(user, pass)
      }
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
    import com.beardedlogic.shipreq.app.AppConfig._
    logger.info(s"Signup allowed: ${AllowRegister()}")
  }
}
