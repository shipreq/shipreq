package shipreq.webapp.snippet

import java.sql.Connection
import net.liftweb.http.S
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import org.joda.time.DateTime

import shipreq.taskman.api.Msg
import shipreq.webapp.app.AppConfig.PasswordResetTokenLifespan
import shipreq.webapp.db.{DaoT, UserRegistrationInfo, ResetPasswordInfo}
import shipreq.webapp.feature.validation.{ValidationResultT, Validators}
import shipreq.webapp.lib.{FormVar, SingleOpStatefulSnippet, SnippetHelpers}
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
    val emailV = FormVar.strOnSubmit(Validators.email, "#email")("")

    def onSubmit(): JsCmd = {
      securityProvider.enforceHumanSpeed()
      perform(emailV.validate)
    }

    emailV.csssel & ":submit" #> ajaxSubmitOnClick(onSubmit)
  }

  def perform(v: ValidationResultT[String]): JsCmd =
    ifValid(v)(email =>
      daoProvider.withTransactionLevel(Connection.TRANSACTION_SERIALIZABLE)(dao => {
        dao.findUserRegAndResetPwInfo(email) match {

          // No associated account
          case None =>

          // Account not activated yet
          case Some((u@UserRegistrationInfo(_, _, _, None), _)) =>
            taskmanD(dao, _ submitMsg Register1.preRegistrationMsg(email.tag, u, dao))

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

  def passwordResetMsg(email: String @@ InputCorrected, token: String): Msg =
    Msg.PasswordResetRequested(email.tag, AppSiteMap.ResetPassword2.absoluteUrl(token))

  val jsEmailSent: JsCmd =
    JqExpr("#resetpw1Form,#resetpwTokenSent") ~> JqToggle

  private def issueNewToken(id: UserId, dao: DaoT): String =
    dao.performInstallNewResetPasswordToken(id, () => randomConfirmationToken)

  private def reuseToken(id: UserId, token: String, dao: DaoT): Unit =
    dao.performReuseResetPasswordToken(id)
}

// =====================================================================================================================

class ResetPassword2(token: String) extends SingleOpStatefulSnippet {

  val passwordV = FormVar.passwordPair("#password1", "#password2")

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
    passwordV.csssel & ":submit" #> ajaxSubmitOnClick(onSubmit)
  }

  def onSubmit(): JsCmd =
    try
      ifValid(passwordV.validate)(resetPassword)
    finally
      passwordV.fv.set2("") // Let's not keep the plaintext passwords around

  def resetPassword(password: String): JsCmd = {
    val ps = PasswordAndSalt.createWithRandomSalt(password)
    daoProvider.withSession(_.performPasswordReset(ps, token))
    jsClearError & JqExpr("#resetpw2Form,#resetpwComplete") ~> JqToggle
  }
}
