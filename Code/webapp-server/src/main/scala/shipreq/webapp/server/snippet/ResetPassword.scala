package shipreq.webapp.server.snippet

import java.sql.Connection
import net.liftweb.http.S
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import org.joda.time.DateTime
import shipreq.taskman.api.{EmailAddr, Msg, UserId}
import shipreq.webapp.base.validation.ValidationResult
import shipreq.webapp.server.ServerConfig.PasswordResetTokenLifespan
import shipreq.webapp.server.app.AppSiteMap
import shipreq.webapp.server.app.AppSiteMap.Implicits._
import shipreq.webapp.server.data.UserRegistrationInfo
import shipreq.webapp.server.db.{DaoT, ResetPasswordInfo}
import shipreq.webapp.server.feature.validation.Validators
import shipreq.webapp.server.lib.{FormVar, SingleOpStatefulSnippet, SnippetHelpers}
import shipreq.webapp.server.security.PasswordAndSalt
import shipreq.webapp.server.snippet.ResetPassword._
import shipreq.webapp.server.util.HtmlTransformExt.ajaxSubmitOnClick
import shipreq.webapp.server.util.JsExt._

object ResetPassword {
  def isTokenExpired(dateIssued: DateTime): Boolean = PasswordResetTokenLifespan.ago.isAfter(dateIssued)
}

// =====================================================================================================================

object ResetPassword1 extends SnippetHelpers {

  val form = FormVar.strOnSubmit(Validators.email, "#email")

  def render = {
    var vars: form.Var = ""

    def onSubmit(): JsCmd = {
      securityProvider.enforceHumanSpeed()
      perform(form validate vars)
    }

    form.csssel(vars, vars = _) & ":submit" #> ajaxSubmitOnClick(onSubmit)
  }

  def perform(v: ValidationResult[EmailAddr]): JsCmd =
    ifValid(v)(email =>
      daoProvider.withTransactionLevel(Connection.TRANSACTION_SERIALIZABLE)(dao => {
        dao.findUserRegAndResetPwInfo(email) match {

          // No associated account
          case None =>

          // Account not activated yet
          case Some((u@UserRegistrationInfo(_, _, _, None), _)) =>
            taskmanD(dao, _ submitMsg Register1.preRegistrationMsg(email, u, dao))

          // Valid token available
          case Some((UserRegistrationInfo(id, _, _, Some(_)), ResetPasswordInfo(Some(token), Some(issued)))) if !isTokenExpired(issued) =>
            reuseToken(id, token, dao)
            taskmanD(dao, _ submitMsg passwordResetMsg(email, token))

          // No token or token expired
          case Some((UserRegistrationInfo(id, _, _, Some(_)), _)) =>
            val token = issueNewToken(id, dao)
            taskmanD(dao, _ submitMsg passwordResetMsg(email, token))
        }

      // Respond the same in all cases (for security purposes)
      jsEmailSent
    })
  )

  def passwordResetMsg(email: EmailAddr, token: String): Msg =
    Msg.PasswordResetRequested(email, AppSiteMap.ResetPassword2.absoluteUrl(token))

  val jsEmailSent: JsCmd =
    JqExpr("#resetpw1Form,#resetpwTokenSent") ~> JqToggle

  private def issueNewToken(id: UserId, dao: DaoT): String =
    dao.performInstallNewResetPasswordToken(id, () => randomConfirmationToken)

  private def reuseToken(id: UserId, token: String, dao: DaoT): Unit =
    dao.performReuseResetPasswordToken(id)
}

// =====================================================================================================================

object ResetPassword2 {
  val form = FormVar.passwordPair("#password1", "#password2")
}

class ResetPassword2(token: String) extends SingleOpStatefulSnippet {
  import ResetPassword2._

  var vars: form.Var = FormVar.emptyPasswordPair

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
    form.csssel(vars, vars = _) & ":submit" #> ajaxSubmitOnClick(onSubmit)
  }

  def onSubmit(): JsCmd =
    try
      ifValid(form validate vars)(resetPassword)
    finally
      vars = FormVar.emptyPasswordPair // Let's not keep the plaintext passwords around

  def resetPassword(password: String): JsCmd = {
    val ps = PasswordAndSalt.createWithRandomSalt(password)
    daoProvider.withSession(_.performPasswordReset(ps, token))
    jsClearError & JqExpr("#resetpw2Form,#resetpwComplete") ~> JqToggle
  }
}
