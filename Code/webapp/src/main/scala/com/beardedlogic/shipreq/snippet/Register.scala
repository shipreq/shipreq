package com.beardedlogic.shipreq
package snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Helpers._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken
import org.joda.time.DateTime

import app.{AppConfig, AppSiteMap}
import lib.MailHelpers.MailContent
import lib.{SnippetHelpers, SingleOpStatefulSnippet}
import lib.Types._
import mail.RegistrationEmails
import db.{DaoT, UserRegistrationInfo, UserRegistrationResult}
import feature.validation.Validator
import security.{Permissions, PasswordAndSalt}
import util.JsExt._
import util.HtmlTransformExt.ajaxSubmitOnClick
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
      val mail: MailContent = daoProvider.withTransaction(dao =>
        dao.findUserRegistrationInfo(emailAddr) match {
          case None    => onNewUser(emailAddr, dao)
          case Some(u) => performPreRegistation(u, dao)
        }
      )
      sendMail(mail addressedTo emailAddr)
      jsClearError & JqExpr("#emailSent,#register1Form") ~> JqToggle
    })

  def performPreRegistation(u: UserRegistrationInfo, dao: DaoT): MailContent =
    u match {
      case UserRegistrationInfo(_, _, _, Some(_)) =>
        onAlreadyRegistered()
      case UserRegistrationInfo(_, Some(token), Some(issued), None) if !isTokenExpired(issued) =>
        onTokenReusable(token)
      case UserRegistrationInfo(id, _, _, None) =>
        onTokenExpired(id, dao)
    }

  // TODO add retries incase token in use

  private def onNewUser(email: String @@ Validated, dao: DaoT): MailContent = {
    val token = randomConfirmationToken
    dao.createUserPlaceholder(email, token)
    RegistrationEmails.LinkToCompleteRegistration(token)
  }

  private def onTokenReusable(token: String): MailContent = {
    RegistrationEmails.LinkToCompleteRegistration(token)
  }

  private def onTokenExpired(id: UserId, dao: DaoT): MailContent = {
    val token = randomConfirmationToken
    dao.updateUserConfirmationToken(id, token)
    RegistrationEmails.LinkToCompleteRegistration(token)
  }

  private def onAlreadyRegistered() = RegistrationEmails.AlreadyRegistered
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

    val v = Validator.Ap.apply2(
      Validator.username.correctAndValidate(usernameInput),
      Validator.passwords.correctAndValidate(password1Input, password2Input)
    )(Tuple2.apply)

    ifValid(v)(r => {
      val (username, password) = r
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
