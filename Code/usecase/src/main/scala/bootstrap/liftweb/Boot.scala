package bootstrap.liftweb

import net.liftweb.http._
import net.liftmodules.scamljade.ScamlJade
import net.liftweb.util.{Props, Mailer}
import javax.mail.{Authenticator, PasswordAuthentication}

import com.beardedlogic.usecase._
import lib.Defaults
import lib.db.DB
import lib.security.Oshiro
import app.AppSiteMap
import scala.slick.session.Session
import com.beardedlogic.usecase.model.FieldKeyType

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {

  LiftRules.configureLogging()

  def boot {
    configureLift
    initDatabase
  }

  def configureLift() {

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

  def initDatabase() {
    def initDbModels(): Unit = DB.Slick.withTransaction { implicit s: Session =>
      FieldKeyType.init
    }

    DB.init()
    initDbModels()
    Defaults.init()
  }

  def initMailer() {
    Mailer.authenticator = for {
      user <- Props.get("mail.user")
      pass <- Props.get("mail.password")
    } yield new Authenticator {
        override def getPasswordAuthentication = new PasswordAuthentication(user, pass)
      }
  }
}
