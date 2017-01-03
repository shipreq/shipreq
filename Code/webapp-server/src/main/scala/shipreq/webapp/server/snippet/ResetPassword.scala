package shipreq.webapp.server.snippet

import doobie.imports.ConnectionIO
import java.sql.Connection
import java.time.Instant
import net.liftweb.http.S
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import scalaz.Free
import scalaz.effect.IO
import shipreq.base.db.DoobieHelpers._
import shipreq.taskman.api.{EmailAddr, Msg, UserId}
import shipreq.webapp.base.validation.ValidationResult
import shipreq.webapp.server.app.{AppSiteMap, DI}
import shipreq.webapp.server.app.AppSiteMap.Implicits._
import shipreq.webapp.server.data.{ResetPasswordInfo, UserRegistrationInfo}
import shipreq.webapp.server.db.DbLogic
import shipreq.webapp.server.feature.validation.Validators
import shipreq.webapp.server.lib.{FormVar, Misc, SingleOpStatefulSnippet, SnippetHelpers}
import shipreq.webapp.server.security.PasswordAndSalt
import shipreq.webapp.server.snippet.ResetPassword._
import shipreq.webapp.server.util.HtmlTransformExt.ajaxSubmitOnClick
import shipreq.webapp.server.util.JsExt._

object ResetPassword {
  def isTokenExpired(dateIssued: Instant): Boolean =
    Misc.isExpired_?(dateIssued, DI.serverConfig.passwordResetTokenLifespan)
}

// =====================================================================================================================

object ResetPassword1 extends SnippetHelpers {

  val form = FormVar.strOnSubmit(Validators.email, "#email")

  def render = {
    var vars: form.Var = ""

    def onSubmit(): JsCmd = {
      securityProvider().enforceHumanSpeed()
      perform(form validate vars)
    }

    form.csssel(vars, vars = _) & ":submit" #> ajaxSubmitOnClick(onSubmit)
  }

  def perform(v: ValidationResult[EmailAddr]): JsCmd =
    ifValid(v) { email =>

      val dbPlan = resetLogic(email).withTransactionLevel(Connection.TRANSACTION_SERIALIZABLE)
      val plan = db().io.trans(dbPlan).flatMap {
        case Some(msg) => taskman().submitMsg(msg).map(_ => ())
        case None => IO.ioUnit
      }
      plan.unsafePerformIO()

      // Respond the same in all cases (for security purposes)
      jsEmailSent
    }

  def resetLogic(email: EmailAddr): ConnectionIO[Option[Msg]] =
    DbLogic.user.findRegAndResetPwInfo(email) flatMap {

      // No associated account
      case None =>
        Free pure None

      // Account not activated yet
      case Some((u@UserRegistrationInfo(_, _, _, None), _)) =>
        Register1.preRegistrationMsg(email, u).map(Some(_))

      // Valid token available
      case Some((UserRegistrationInfo(id, _, _, Some(_)), ResetPasswordInfo(Some(token), Some(issued)))) if !isTokenExpired(issued) =>
        reuseToken(id).map(_ => Some(passwordResetMsg(email, token)))

      // No token or token expired
      case Some((UserRegistrationInfo(id, _, _, Some(_)), _)) =>
        issueNewToken(id).map(token => Some(passwordResetMsg(email, token)))
    }

  def passwordResetMsg(email: EmailAddr, token: String): Msg =
    Msg.PasswordResetRequested(email, AppSiteMap.ResetPassword2.absoluteUrl(token))

  val jsEmailSent: JsCmd =
    JqExpr("#resetpw1Form,#resetpwTokenSent") ~> JqToggle

  private def issueNewToken(id: UserId): ConnectionIO[String] =
    DbLogic.user.performInstallNewResetPasswordToken(id, () => randomConfirmationToken())

  private def reuseToken(id: UserId): ConnectionIO[Unit] =
    DbLogic.user.performReuseResetPasswordToken(id)
}

// =====================================================================================================================

object ResetPassword2 {
  val form = FormVar.passwordPair("#password1", "#password2")
}

class ResetPassword2(token: String) extends SingleOpStatefulSnippet {
  import ResetPassword2._

  var vars: form.Var = FormVar.emptyPasswordPair

  def validateToken_!(): Unit =
    db().io.trans(DbLogic.user.findResetPasswordTokenIssuedDate(token)).unsafePerformIO() match {
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
    db().io.trans(DbLogic.user.performPasswordReset(ps, token)).unsafePerformIO()
    jsClearError & JqExpr("#resetpw2Form,#resetpwComplete") ~> JqToggle
  }
}
