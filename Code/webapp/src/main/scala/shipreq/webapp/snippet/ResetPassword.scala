package shipreq.webapp.snippet

import java.sql.Connection
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import org.joda.time.DateTime

import shipreq.taskman.api.TaskDef
import shipreq.webapp.app.AppConfig.PasswordResetTokenLifespan
import shipreq.webapp.db.{DaoT, UserRegistrationInfo, ResetPasswordInfo}
import shipreq.webapp.feature.validation.Validator
import shipreq.webapp.lib.{SingleOpStatefulSnippet, SnippetHelpers}
import shipreq.webapp.lib.Types._
import shipreq.webapp.util.HtmlTransformExt.ajaxSubmitOnClick
import shipreq.webapp.util.JsExt._
import shipreq.webapp.app.AppSiteMap
import shipreq.webapp.security.PasswordAndSalt
import AppSiteMap.Implicits._
import ResetPassword._

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

  def perform(emailInput: String): JsCmd =
    ifValid(Validator.email.correctAndValidate(emailInput))(email =>
      daoProvider.withTransactionLevel(Connection.TRANSACTION_SERIALIZABLE)(dao => {
        dao.findUserRegAndResetPwInfo(email) match {

          // No associated account
          case None =>

          // Account not activated yet
          case Some((u@UserRegistrationInfo(_, _, _, None), _)) =>
            submitTask(Register1.preRegistrationTask(email.tag, u, dao), dao)

          // Valid token available
          case Some((UserRegistrationInfo(id, _, _, Some(_)), ResetPasswordInfo(Some(token), Some(issued)))) if !isTokenExpired(issued) =>
            reuseToken(id, token, dao)
            submitTask(passwordResetTask(email, token), dao)

          // No token or token expired
          case Some((UserRegistrationInfo(id, _, _, Some(_)), _)) =>
            val token = issueNewToken(id, dao)
            submitTask(passwordResetTask(email, token), dao)
        }

      // Respond the same in all cases (for security purposes)
      jsEmailSent
    })
  )

  def passwordResetTask(email: String @@ InputCorrected, token: String): TaskDef =
    TaskDef.PasswordResetRequested(email.tag, AppSiteMap.ResetPassword2.absoluteUrl(token))

  val jsEmailSent: JsCmd =
    jsClearError & JqExpr("#resetpw1Form,#resetpwTokenSent") ~> JqToggle

  private def issueNewToken(id: UserId, dao: DaoT): String =
    dao.performInstallNewResetPasswordToken(id, () => randomConfirmationToken)

  private def reuseToken(id: UserId, token: String, dao: DaoT): Unit =
    dao.performReuseResetPasswordToken(id)
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
