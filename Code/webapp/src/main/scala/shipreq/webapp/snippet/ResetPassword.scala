package shipreq.webapp
package snippet

import java.sql.Connection
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import org.joda.time.DateTime

import app.AppConfig.PasswordResetTokenLifespan
import db.{DaoT, UserRegistrationInfo, ResetPasswordInfo}
import feature.validation.Validator
import lib.MailHelpers.MailContent
import lib.{SingleOpStatefulSnippet, SnippetHelpers}
import lib.Types._
import mail.PasswordResetEmails
import util.HtmlTransformExt.ajaxSubmitOnClick
import util.JsExt._
import ResetPassword._
import app.AppSiteMap
import security.PasswordAndSalt

object ResetPassword {
  def isTokenExpired(dateIssued: DateTime): Boolean = PasswordResetTokenLifespan.ago.isAfter(dateIssued)
}

// =====================================================================================================================

object ResetPassword1 extends SnippetHelpers {

  def render = {
    var emailInput = ""

    def onSubmit(): JsCmd = {
      securityProvider.enforceHumanSpeed()
      perform(emailInput)
    }

    (
      "#email" #> SHtml.onSubmit(emailInput = _) &
      ":submit" #> ajaxSubmitOnClick(onSubmit)
    )
  }

  def perform(emailInput: String): JsCmd = {
    val email = Validator.email.correct(emailInput)
    daoProvider.withTransactionLevel(Connection.TRANSACTION_SERIALIZABLE)(dao =>
      dao.findUserRegAndResetPwInfo(email) match {

        case None =>
          ifValid(Validator.email.validate(email))(_ => jsEmailSent)

        case Some((u@UserRegistrationInfo(_, _, _, None), _)) =>
          send(email, Register1.performPreRegistation(u, dao))

        case Some((UserRegistrationInfo(id, _, _, Some(_)), ResetPasswordInfo(Some(token), Some(issued)))) if !isTokenExpired(issued) =>
          send(email, reuseToken(id, token, dao))

        case Some((UserRegistrationInfo(id, _, _, Some(_)), _)) =>
          send(email, issueNewToken(id, dao))
  })}

  def send(emailAddr: String, mail: MailContent): JsCmd = {
    sendMail(mail addressedTo emailAddr)
    jsEmailSent
  }

  val jsEmailSent: JsCmd =
    jsClearError & JqExpr("#resetpw1Form,#resetpwTokenSent") ~> JqToggle

  private def issueNewToken(id: UserId, dao: DaoT): MailContent = {
    val token = dao.performInstallNewResetPasswordToken(id, () => randomConfirmationToken)
    PasswordResetEmails.PasswordChangeRequest(token)
  }

  private def reuseToken(id: UserId, token: String, dao: DaoT): MailContent = {
    dao.performReuseResetPasswordToken(id)
    PasswordResetEmails.PasswordChangeRequest(token)
  }
}

// =====================================================================================================================

class ResetPassword2(token: String) extends SingleOpStatefulSnippet {

  var usernameInput = ""
  var password1Input = ""
  var password2Input = ""

  def validateToken_!(): Unit =
    daoProvider.withSession(_ findResetPasswordTokenIssuedDate token) match {
      case None =>
        S.error("The token associated with that URL is invalid.")
        redirectTo(AppSiteMap.Login)

      case Some(issued) if isTokenExpired(issued) =>
        S.error("Your password-reset token has expired. Please re-enter your email address to get a new token.")
        redirectTo(AppSiteMap.ResetPassword1)

      case _ => // valid
    }

  def render = {
    validateToken_!()
    (
      "#password1" #> SHtml.onSubmit(password1Input = _) &
      "#password2" #> SHtml.onSubmit(password2Input = _) &
      ":submit" #> ajaxSubmitOnClick(onSubmit)
    )
  }

  def onSubmit(): JsCmd =
    try {
      ifValid(Validator.passwords.correctAndValidate(password1Input, password2Input))(resetPassword)
    } finally {
      password1Input = "" // Let's not keep the plaintext passwords around
      password2Input = ""
    }

  def resetPassword(password: String): JsCmd = {
    val ps = PasswordAndSalt.createWithRandomSalt(password)
    daoProvider.withSession(_.performPasswordReset(ps, token))
    jsClearError & JqExpr("#resetpw2Form,#resetpwComplete") ~> JqToggle
  }
}
