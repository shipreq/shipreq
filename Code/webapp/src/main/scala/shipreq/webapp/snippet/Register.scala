package shipreq.webapp.snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.http.S
import net.liftweb.util.Helpers._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken
import org.joda.time.DateTime

import shipreq.taskman.api.Msg
import shipreq.webapp.app.{AppConfig, AppSiteMap}
import shipreq.webapp.lib.{FormVar, SnippetHelpers, SingleOpStatefulSnippet}
import shipreq.webapp.lib.Types._
import shipreq.webapp.db.{DaoT, UserRegistrationInfo, UserRegistrationResult}
import shipreq.webapp.feature.validation.{ValidationResult, Validators}
import shipreq.webapp.security.{Permissions, PasswordAndSalt}
import shipreq.webapp.util.JsExt._
import shipreq.webapp.util.HtmlTransformExt.ajaxSubmitOnClick
import AppSiteMap.Implicits._
import Msg.{ReRegistrationAttempted, RegistrationRequested}
import Register._

object Register {
  def isTokenExpired(dateIssued: DateTime): Boolean = AppConfig.TokenLifespan.ago.isAfter(dateIssued)
}

/**
 * Takes an email address, validates it, creates a new user, sends an email with a verification-token in it.
 *
 * @since 27/06/2013
 */
object Register1 extends SnippetHelpers {

  def render = {
    val emailV = FormVar.strOnSubmit(Validators.emailEA, "#email")("")

    def onSubmit(): JsCmd = {
      securityProvider.enforceHumanSpeed()
      perform(emailV.validate)
    }

    if (Permissions.userRegistration.using().isPass)
      ( "#registrationDisabled" #> ""
      & emailV.csssel
      & ":submit" #> ajaxSubmitOnClick(onSubmit))
    else
      "#register1Form" #> ""
  }

  def perform(v: ValidationResult[EmailAddr]): JsCmd =
    ifValid(v)(emailAddr => {
      daoProvider.withTransaction(dao => {
        val msg = dao.findUserRegistrationInfo(emailAddr) match {
          case None    => onNewUser(emailAddr, dao)
          case Some(u) => preRegistrationMsg(emailAddr, u, dao)
        }
        taskmanD(dao, _ submitMsg msg)
      })
      jsClearError & JqExpr("#emailSent,#register1Form") ~> JqToggle
    })

  def preRegistrationMsg(email: EmailAddr, u: UserRegistrationInfo, dao: DaoT): Msg =
    u match {
      case UserRegistrationInfo(_, _, _, Some(_)) =>
        onAlreadyRegistered(email)
      case UserRegistrationInfo(_, Some(token), Some(issued), None) if !isTokenExpired(issued) =>
        onTokenReusable(email, token)
      case UserRegistrationInfo(id, _, _, None) =>
        onTokenExpired(email, id, dao)
    }

  private def onNewUser(email: EmailAddr, dao: DaoT): Msg = {
    val token = dao.createUserPlaceholder(email, () => randomConfirmationToken)
    registrationRequestedTask(email, token)
  }

  private def onTokenReusable(email: EmailAddr, token: String): Msg =
    registrationRequestedTask(email, token)

  private def onTokenExpired(email: EmailAddr, id: UserId, dao: DaoT): Msg = {
    val token = dao.updateUserConfirmationToken(id, () => randomConfirmationToken)
    registrationRequestedTask(email, token)
  }

  private def onAlreadyRegistered(email: EmailAddr): Msg =
    ReRegistrationAttempted(email)

  private def registrationRequestedTask(email: EmailAddr, token: String): Msg =
    RegistrationRequested(email, AppSiteMap.Register2.absoluteUrl(token))
}

/**
 * Validates a token from email (part of the URL) and presents the user with a username/password form. Upon form
 * submission the user account is activated.
 *
 * @since 1/07/2013
 */
class Register2(token: String) extends SingleOpStatefulSnippet {

  val nameV       = FormVar.strOnSubmit(Validators.landingPage.name, "#name")("")
  val usernameV   = FormVar.ajaxStr(Validators.user.username, JqId("username"))("")
  val passwordV   = FormVar.passwordPair("#password1", "#password2")
  val newsletterV = FormVar.boolOnSubmit("#newsletter")(true)
  val tosV        = FormVar.boolOnSubmit(Validators.tosAgreement, "#tos")(false)
  val vars = FormVar.AP5(nameV, usernameV, passwordV, newsletterV, tosV)

  def render = {
    securityProvider.enforceHumanSpeed()
    validateToken_!()
    vars.csssel & ":submit" #> ajaxSubmitOnClick(onSubmit)
  }

  def validateToken_!(): Unit =
    daoProvider.withSession(_.findUserConfirmationTokenIssuedDate(token)) match {
      case None =>
        S.error("Invalid registration token. Please re-register your email address.")
        redirectTo(AppSiteMap.Register1)

      case Some(issued) if isTokenExpired(issued) =>
        S.error("Your registration token has expired. Please re-register your email address to get a new token.")
        redirectTo(AppSiteMap.Register1)

      case _ => // valid
    }

  def onSubmit(): JsCmd = try {
    import UserRegistrationResult._

    ifValid(vars.validate(Tuple5.apply))(r => {
      val (name, username, password, newsletter, _) = r
      val ps = PasswordAndSalt.createWithRandomSalt(password)

      daoProvider.withTransaction(
        _.performUserRegistration(token)(username, ps, clientIp.getOrElse("?"))(name, newsletter)
      ) match {

        case UsernameTaken => jsShowError("Username is already taken.")

        case NoMatchingConfToken =>
          S.error("Your registration token disappeared.")
          redirectTo(AppSiteMap.Login)

        // Registration complete
        case DbSuccess(id) =>
          info(s"Registered new user: $username")
          taskman1(_ submitMsg Msg.RegistrationCompleted(id))
          SecurityUtils.getSubject.login(new UsernamePasswordToken(username, password))
          jsClearError & JqExpr("#regComplete,#register2") ~> JqToggle
      }
    })
  } finally
    passwordV.fv.set2("") // Let's not keep the plaintext passwords around
}
