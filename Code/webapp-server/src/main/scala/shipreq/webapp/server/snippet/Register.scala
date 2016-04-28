package shipreq.webapp.server.snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.http.S
import net.liftweb.util.Helpers._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken
import org.joda.time.DateTime
import shipreq.base.util.ScalaExt._
import shipreq.taskman.api.{EmailAddr, Msg, UserId}
import shipreq.webapp.base.validation.ValidationResult
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.app.AppSiteMap
import shipreq.webapp.server.data.UserRegistrationInfo
import shipreq.webapp.server.lib.{FormVar, SingleOpStatefulSnippet, SnippetHelpers}
import shipreq.webapp.server.db.{DaoT, UserRegistrationResult}
import shipreq.webapp.server.feature.validation.Validators
import shipreq.webapp.server.security.{PasswordAndSalt, Permissions}
import shipreq.webapp.server.util.JsExt._
import shipreq.webapp.server.util.HtmlTransformExt.ajaxSubmitOnClick
import AppSiteMap.Implicits._
import Msg.{ReRegistrationAttempted, RegistrationRequested}
import Register._

object Register {
  def isTokenExpired(dateIssued: DateTime): Boolean = ServerConfig.TokenLifespan.ago.isAfter(dateIssued)
}

// =====================================================================================================================

/**
 * Takes an email address, validates it, creates a new user, sends an email with a verification-token in it.
 *
 * @since 27/06/2013
 */
object Register1 extends SnippetHelpers {

  val form = FormVar.strOnSubmit(Validators.email, "#email")

  def render = {
    var vars: form.Var = ""

    def onSubmit(): JsCmd = {
      securityProvider.enforceHumanSpeed()
      perform(form validate vars)
    }

    if (Permissions.userRegistration.using().isPass)
      ( "#registrationDisabled" #> ""
      & form.csssel(vars, vars = _)
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
      JqExpr("#emailSent,#register1Form") ~> JqToggle
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

// =====================================================================================================================

object Register2 {
  val form = FormVar.merge(
    FormVar.strOnSubmit (Validators.user.name, "#name"),
    FormVar.ajaxStr     (Validators.user.username, JqId("username")),
    FormVar.passwordPair("#password1", "#password2"),
    FormVar.boolOnSubmit("#newsletter"),
    FormVar.boolOnSubmit(Validators.tosAgreement, "#tos")
  )(Tuple5.apply)
}

/**
 * Validates a token from email (part of the URL) and presents the user with a username/password form. Upon form
 * submission the user account is activated.
 *
 * @since 1/07/2013
 */
class Register2(token: String) extends SingleOpStatefulSnippet {
  import Register2._

  var vars: form.Var = ("", "", FormVar.emptyPasswordPair, true, false)

  def render = {
    securityProvider.enforceHumanSpeed()
    validateToken_!()
    form.csssel(vars, vars = _) & ":submit" #> ajaxSubmitOnClick(onSubmit)
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

    ifValid(form validate vars)(r => {
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
          log.info(s"Registered new user: $username")
          taskman1(_ submitMsg Msg.RegistrationCompleted(id))
          SecurityUtils.getSubject.login(new UsernamePasswordToken(username.value, password))
          JqExpr("#regComplete,#register2") ~> JqToggle
      }
    })
  } finally
    vars = vars put3 FormVar.emptyPasswordPair // Let's not keep the plaintext passwords around
}
