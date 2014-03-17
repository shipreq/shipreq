package shipreq.webapp.snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Helpers._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken
import org.joda.time.DateTime

import shipreq.taskman.api.TaskDef
import shipreq.webapp.app.{AppConfig, AppSiteMap}
import shipreq.webapp.lib.{SnippetHelpers, SingleOpStatefulSnippet}
import shipreq.webapp.lib.Types._
import shipreq.webapp.db.{DaoT, UserRegistrationInfo, UserRegistrationResult}
import shipreq.webapp.feature.validation.Validator
import shipreq.webapp.security.{Permissions, PasswordAndSalt}
import shipreq.webapp.util.JsExt._
import shipreq.webapp.util.HtmlTransformExt.ajaxSubmitOnClick
import AppSiteMap.Implicits._
import TaskDef.{ReRegistrationAttempted, RegistrationRequested}
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
    var emailInput = ""

    def onSubmit(): JsCmd = {
      securityProvider.enforceHumanSpeed()
      perform(emailInput)
    }

    if (Permissions.userRegistration.using().isPass)
      ("#registrationDisabled" #> "" &
        "#email" #> SHtml.onSubmit(emailInput = _) &
        ":submit" #> ajaxSubmitOnClick(onSubmit))
    else
      "#register1Form" #> ""
  }

  def perform(emailInput: String): JsCmd =
    ifValid(Validator.email.correctAndValidate(emailInput))(emailAddr => {
      daoProvider.withTransaction(dao => {
        val task = dao.findUserRegistrationInfo(emailAddr) match {
          case None    => onNewUser(emailAddr, dao)
          case Some(u) => preRegistrationTask(emailAddr, u, dao)
        }
        submitTask(task, dao)
      })
      jsClearError & JqExpr("#emailSent,#register1Form") ~> JqToggle
    })

  def preRegistrationTask(email: String @@ Validated, u: UserRegistrationInfo, dao: DaoT): TaskDef =
    u match {
      case UserRegistrationInfo(_, _, _, Some(_)) =>
        onAlreadyRegistered(email)
      case UserRegistrationInfo(_, Some(token), Some(issued), None) if !isTokenExpired(issued) =>
        onTokenReusable(email, token)
      case UserRegistrationInfo(id, _, _, None) =>
        onTokenExpired(email, id, dao)
    }

  private def onNewUser(email: String @@ Validated, dao: DaoT): TaskDef = {
    val token = dao.createUserPlaceholder(email, () => randomConfirmationToken)
    registrationRequestedTask(email, token)
  }

  private def onTokenReusable(email: String @@ Validated, token: String): TaskDef =
    registrationRequestedTask(email, token)

  private def onTokenExpired(email: String @@ Validated, id: UserId, dao: DaoT): TaskDef = {
    val token = dao.updateUserConfirmationToken(id, () => randomConfirmationToken)
    registrationRequestedTask(email, token)
  }

  private def onAlreadyRegistered(email: String @@ Validated): TaskDef =
    ReRegistrationAttempted(email.tag, AppSiteMap.Login.absoluteUrl)

  private def registrationRequestedTask(email: String @@ Validated, token: String): TaskDef =
    RegistrationRequested(email.tag, AppSiteMap.Register2.absoluteUrl(token))
}

/**
 * Validates a token from email (part of the URL) and presents the user with a username/password form. Upon form
 * submission the user account is activated.
 *
 * @since 1/07/2013
 */
class Register2(token: String) extends SingleOpStatefulSnippet {

  var usernameInput = ""
  var password1Input = ""
  var password2Input = ""
  var tos = false

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

  def render = {
    securityProvider.enforceHumanSpeed()
    validateToken_!()
    (
      "#username" #> SHtml.ajaxText(usernameInput, onUsernameChange)
        & "#password1" #> SHtml.onSubmit(password1Input = _)
        & "#password2" #> SHtml.onSubmit(password2Input = _)
        & "#tos" #> SHtml.onSubmitBoolean(tos = _)
        & ":submit" #> ajaxSubmitOnClick(onSubmit)
      )
  }

  // TODO onUsernameChange(): This should be pure JS
  def onUsernameChange(input: String): JsCmd = {
    usernameInput = Validator.username.correct(input)
    JqId("username") ~> JqSetValue(usernameInput)
  }

  def onSubmit(): JsCmd = try {
    import UserRegistrationResult._

    val v = Validator.Ap.apply3(
      Validator.username.correctAndValidate(usernameInput),
      Validator.passwords.correctAndValidate(password1Input, password2Input),
      Validator.tosAgreement.correctAndValidate(tos)
    )(Tuple3.apply)

    ifValid(v)(r => {
      val (username, password, _) = r
      val ps = PasswordAndSalt.createWithRandomSalt(password)
      daoProvider.withSession(_.performUserRegistration(token)(username, ps, clientIp.getOrElse("?"))) match {

        case UsernameTaken => jsShowError("Username is already taken.")

        case NoMatchingConfToken =>
          S.error("Your registration token disappeared.")
          redirectTo(AppSiteMap.Login)

        // Registration complete
        case DbSuccess(_) =>
          info(s"Registered new user: $username")
          SecurityUtils.getSubject.login(new UsernamePasswordToken(username, password))
          jsClearError & JqExpr("#regComplete,#register2") ~> JqToggle
      }
    })
  } finally {
    password1Input = "" // Let's not keep the plaintext passwords around
    password2Input = ""
  }
}
