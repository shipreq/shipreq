package com.beardedlogic.usecase
package snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Helpers._
import net.liftweb.util.Mailer._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken

import app.AppSiteMap
import AppSiteMap.Implicits._
import lib._
import Types._
import mail.RegistrationEmails
import db.{Dao, UserRegistrationInfo, UserRegistrationResult}
import security.PasswordAndSalt
import util.JsExt._
import util.HtmlTransformExt.ajaxSubmitOnClick

/**
 * Takes an email address, validates it, creates a new user, sends an email with a verification-token in it.
 *
 * @since 27/06/2013
 */
class Register1 extends SingleOpStatefulSnippet {

  var emailInput = ""

  def render = (
    "#email" #> SHtml.onSubmit(emailInput = _)
      & ":submit" #> ajaxSubmitOnClick(onSubmit)
    )

  def onSubmit(): JsCmd = {
    val email = InputCorrection.email(emailInput)
    Validate.email(email) match {
      case Some(errmsg) => jsShowError(errmsg)
      case None =>
        val mail: Mail = daoProvider.withTransaction(dao =>
          dao.findUserRegistrationInfo(email) match {
            case None => onNewUser(email, dao)
            case Some(UserRegistrationInfo(_, _, _, Some(_))) => onAlreadyRegistered()
            case Some(UserRegistrationInfo(_, Some(token), Some(issued), _)) if !isConfirmationTokenExpired_?(issued) => onTokenReusable(token)
            case Some(UserRegistrationInfo(id, _, _, _)) => onTokenExpired(id, dao)
          }
        )
        sendMail(mail, To(email))
        jsClearError() & JqExpr("#emailSent,#register1Form") ~> JqToggle
    }
  }

  private def onNewUser(email: String, dao: Dao): Mail = {
    val token = randomConfirmationToken
    dao.createUser(email, token)
    RegistrationEmails.LinkToCompleteRegistration(token)
  }

  private def onTokenReusable(token: String): Mail = {
    RegistrationEmails.LinkToCompleteRegistration(token)
  }

  private def onTokenExpired(id: UserId, dao: Dao): Mail = {
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
        S.redirectTo(AppSiteMap.Register1.relativeUrl)

      case Some(issued) if isConfirmationTokenExpired_?(issued) =>
        S.error("Your registration token has expired. Please re-register your email address to get a new token.")
        S.redirectTo(AppSiteMap.Register1.relativeUrl)

      case _ => // valid
    }

  def render = {
    validateToken_!
    (
      "#username" #> SHtml.ajaxText(usernameInput, onUsernameChange)
        & "#password1" #> SHtml.onSubmit(password1Input = _)
        & "#password2" #> SHtml.onSubmit(password2Input = _)
        & ":submit" #> ajaxSubmitOnClick(onSubmit)
      )
  }

  // TODO onUsernameChange(): This should be pure JS
  def onUsernameChange(input: String): JsCmd = {
    usernameInput = InputCorrection.username(input)
    JqId("username") ~> JqSetValue(usernameInput)
  }

  def onSubmit(): JsCmd = {
    val username = InputCorrection.username(usernameInput)
    val password1 = InputCorrection.password(password1Input)
    val password2 = InputCorrection.password(password2Input)
    password1Input = "" // Let's not keep the plaintext passwords around
    password2Input = ""

    val failures = List(
      Validate.username(username)
      , Validate.password(password1)
      , Validate.password2(password1, password2)
    ).filter(_.isDefined).map(_.get)

    if (failures.nonEmpty)
      jsShowErrors(failures)
    else {
      import UserRegistrationResult._

      // Update user
      val ps = PasswordAndSalt.hashWithRandomSalt(password1)
      daoProvider.withSession(_.registerUser(token)(username, ps, clientIp_Or_?)) match {

        case UsernameTaken => jsShowError("Username is already taken.")

        case NoMatchingConfToken =>
          S.error("Your registration token disappeared.")
          S.redirectTo(AppSiteMap.Login.relativeUrl)

        // Registration complete
        case Success(_) =>
          info(s"Registered new user: $username")
          SecurityUtils.getSubject.login(new UsernamePasswordToken(username, password1))
          jsClearError() & JqExpr("#regComplete,#register2") ~> JqToggle
      }
    }
  }
}
