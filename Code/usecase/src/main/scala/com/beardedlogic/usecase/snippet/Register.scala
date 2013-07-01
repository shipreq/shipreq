package com.beardedlogic.usecase
package snippet

import net.liftweb.http.js.JsCmds
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Helpers._
import net.liftweb.util.Mailer._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken

import app.AppSiteMap
import lib._
import JsExt._
import mail.RegistrationEmails
import model.{DAO, UserRegistrationInfo}
import model.DbOpResult.{NothingUpdated, ConstraintViolation, Success}
import msg.{JavaScript, Reactor}
import security.PasswordAndSalt

/**
 * Takes an email address, validates it, creates a new user, sends an email with a verification-token in it.
 *
 * @since 27/06/2013
 */
class Register1 extends SingleOpStatefulSnippet {

  var emailInput = ""

  def render = (
    "#email" #> SHtml.onSubmit(emailInput = _)
      // & "form *+" #> SHtml.hidden(jsCallback(onSubmit(_)))
      & ":submit" #> SHtml.ajaxSubmit("Create Account", jsCallback(onSubmit(_)))
    )

  def onSubmit(implicit reactor: Reactor) {
    val email = InputCorrection.email(emailInput)
    Validate.email(email) match {
      case Some(errmsg) => error(errmsg)
      case None =>
        val mail: Mail = daoProvider.withTransaction(dao =>
          dao.findUserRegistrationInfo(email) match {
            case None => onNewUser(email, dao)
            case Some(UserRegistrationInfo(_, _, _, Some(_))) => onAlreadyRegistered()
            case Some(UserRegistrationInfo(_, Some(token), Some(issued), _)) if !isConfirmationTokenExpired_?(issued) => onTokenReusable(token)
            case Some(UserRegistrationInfo(id, _, _, _)) => onTokenExpired(id, dao)
          }
        )
        reactor(JavaScript)(JqExpr("#emailSent,#register1Form") ~> JqToggle)
        sendMail(mail, To(email))
    }
  }

  private def error(errMsg: String)(implicit reactor: Reactor) {
    reactor(JavaScript)(JsCmds.Alert(errMsg))
  }

  private def onNewUser(email: String, dao: DAO): Mail = {
    val token = randomConfirmationToken
    dao.createUser(email, token)
    RegistrationEmails.LinkToCompleteRegistration(token)
  }

  private def onTokenReusable(token: String): Mail = {
    RegistrationEmails.LinkToCompleteRegistration(token)
  }

  private def onTokenExpired(id: Long, dao: DAO): Mail = {
    val token = randomConfirmationToken
    dao.updateUserConfirmationToken(id, token)
    RegistrationEmails.LinkToCompleteRegistration(token)
  }

  private def onAlreadyRegistered() = RegistrationEmails.AlreadyRegistered
}

/**
 *
 * @since 1/07/2013
 */
class Register2(token: String) extends SingleOpStatefulSnippet {

  var usernameInput = ""
  var password1Input = ""
  var password2Input = ""

  def render = {
    validateToken()
    "" #> ""
  }

  def validateToken(): Unit =
    daoProvider.withSession(_.findUserConfirmationTokenIssuedDate(token)) match {
      case None =>
        S.error("Invalid registration token. Please re-register your email address.")
        S.redirectTo(AppSiteMap.Register1.loc.calcDefaultHref)

      case Some(issued) if isConfirmationTokenExpired_?(issued) =>
        S.error("Your registration token has expired. Please re-register your email address to get a new token.")
        S.redirectTo(AppSiteMap.Register1.loc.calcDefaultHref)

      case _ => // valid
    }

  def onSubmit(implicit reactor: Reactor) {
    val username = InputCorrection.username(usernameInput)
    val password1 = InputCorrection.password(password1Input); password1Input = ""
    val password2 = InputCorrection.password(password2Input); password2Input = ""

    val failures = List(
      Validate.username(username)
      , Validate.password(password1)
      , Validate.password2(password1, password2)
    ).filter(_.isDefined).map(_.get)

    if (failures.nonEmpty)
      error(failures.mkString("\n"))
    else {
      // Update user
      val ps = PasswordAndSalt.hashWithRandomSalt(password1)
      daoProvider.withSession(_.registerUser(token)(username, ps, clientIp_Or_?)) match {
        case ConstraintViolation => error("Username is already taken.")
        case NothingUpdated => error("Username is already taken.")
        case Success(_,_) =>
          // Login on success
          SecurityUtils.getSubject.login(new UsernamePasswordToken(username, password1))
        case r => warn("Unexpected result: " + r); shouldNeverHappen_!
      }
    }
  }

  private def error(errMsg: String)(implicit reactor: Reactor) {
    reactor(JavaScript)(JsCmds.Alert(errMsg))
  }
}
