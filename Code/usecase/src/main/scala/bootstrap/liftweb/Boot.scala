package bootstrap.liftweb

import javax.mail.{Authenticator, PasswordAuthentication}
import net.liftweb.http._
import net.liftmodules.scamljade.ScamlJade
import net.liftweb.util.{Props, Mailer}

import com.beardedlogic.usecase._
import app.{Defaults, AppSiteMap}
import db.DB
import security.Oshiro

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {

  LiftRules.configureLogging()

  def boot(): Unit = {
    configureLift()
    preloadTemplates()
    initDatabase()
  }

  def configureLift(): Unit = {

    Oshiro.init()

    initMailer()

    // App package path
    LiftRules.addToPackages("com.beardedlogic.usecase")

    // Register route whitelist
    LiftRules.setSiteMap(AppSiteMap.sitemap)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))

    // Force requests to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

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
    snippet.ShareEditConsts
    snippet.project.UseCaseCrudlConsts
    snippet.uce.Renderer.Templates
  }
}
