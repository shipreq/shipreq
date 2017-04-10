package shipreq.webapp.server.snippet

import doobie.imports.ConnectionIO
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Instant
import net.liftweb.http.S
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken
import scalaz.{Free, \/}
import shipreq.base.db.DoobieHelpers._
import shipreq.taskman.api.Msg.{ReRegistrationAttempted, RegistrationRequested}
import shipreq.taskman.api.{EmailAddr, Msg, UserId}
import shipreq.webapp.base.validation.Composite.Invalidity
import shipreq.webapp.server.app.{AppSiteMap, DI}
import shipreq.webapp.server.app.AppSiteMap.Implicits._
import shipreq.webapp.server.data.UserRegistrationInfo
import shipreq.webapp.server.db.{DbLogic, UserRegistrationResult}
import shipreq.webapp.server.feature.validation.ServerSideValidators
import shipreq.webapp.server.lib.{FormVar, Misc, SingleOpStatefulSnippet, SnippetHelpers}
import shipreq.webapp.server.security.{PasswordAndSalt, Permissions}
import shipreq.webapp.server.snippet.Register._
import shipreq.webapp.server.util.HtmlTransformExt.ajaxSubmitOnClick
import shipreq.webapp.server.util.JsExt._

object Register {
  def isTokenExpired(dateIssued: Instant): Boolean =
    Misc.isExpired_?(dateIssued, DI.serverConfig.tokenLifespan)
}

// =====================================================================================================================

/**
 * Takes an email address, validates it, creates a new user, sends an email with a verification-token in it.
 *
 * @since 27/06/2013
 */
object Register1 extends SnippetHelpers {

  val form = FormVar.strOnSubmit(ServerSideValidators.email.named, "#email")

  def render = {
    var vars: form.Var = ""

    def onSubmit(): JsCmd = {
      securityProvider().enforceHumanSpeed()
      perform(form validate vars)
    }

    if (Permissions.userRegistration.using().isPass)
      ( "#registrationDisabled" #> ""
      & form.csssel(vars, vars = _)
      & ":submit" #> ajaxSubmitOnClick(() => onSubmit()))
    else
      "#register1Form" #> ""
  }

  def perform(v: Invalidity \/ EmailAddr): JsCmd =
    handleCompositeInvalidity(v)(emailAddr => {
      val dbPlan = DbLogic.user.findRegistrationInfo(emailAddr).flatMap {
        case None    => onNewUser(emailAddr)
        case Some(u) => preRegistrationMsg(emailAddr, u)
      }.inTransaction
      val plan = db().io.trans(dbPlan).flatMap(taskman().submitMsg)
      plan.unsafePerformIO()
      JqExpr("#emailSent,#register1Form") ~> JqToggle
    })

  def preRegistrationMsg(email: EmailAddr, u: UserRegistrationInfo): ConnectionIO[Msg] =
    u match {
      case UserRegistrationInfo(_, _, _, Some(_)) =>
        Free pure onAlreadyRegistered(email)
      case UserRegistrationInfo(_, Some(token), Some(issued), None) if !isTokenExpired(issued) =>
        Free pure onTokenReusable(email, token)
      case UserRegistrationInfo(id, _, _, None) =>
        onTokenExpired(email, id)
    }

  private def onNewUser(email: EmailAddr): ConnectionIO[Msg] =
    DbLogic.user.createPlaceholder(email, () => randomConfirmationToken())
      .map(registrationRequestedTask(email, _))

  private def onTokenReusable(email: EmailAddr, token: String): Msg =
    registrationRequestedTask(email, token)

  private def onTokenExpired(email: EmailAddr, id: UserId): ConnectionIO[Msg] =
    DbLogic.user.updateConfirmationToken(id, () => randomConfirmationToken())
      .map(registrationRequestedTask(email, _))

  private def onAlreadyRegistered(email: EmailAddr): Msg =
    ReRegistrationAttempted(email)

  private def registrationRequestedTask(email: EmailAddr, token: String): Msg =
    RegistrationRequested(email, AppSiteMap.Register2.absoluteUrl(token))
}

// =====================================================================================================================

object Register2 {
  val form = FormVar.merge(
    FormVar.strOnSubmit (ServerSideValidators.user.name.named, "#name"),
    FormVar.ajaxStr     (ServerSideValidators.user.username.named, JqId("username")),
    FormVar.passwordPair("#password1", "#password2"),
    FormVar.boolOnSubmit("#newsletter"),
    FormVar.boolOnSubmit(ServerSideValidators.tosAgreement, "#tos")
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
    securityProvider().enforceHumanSpeed()
    validateToken_!()
    form.csssel(vars, vars = _) & ":submit" #> ajaxSubmitOnClick(() => onSubmit())
  }

  def validateToken_!(): Unit =
    db().io.trans(DbLogic.user.findConfirmationTokenIssuedDate(token)).unsafePerformIO() match {
      case None =>
        S.error("Invalid registration token. Please re-register your email address.")
        redirectTo(AppSiteMap.Register1)

      case Some(issued) if isTokenExpired(issued) =>
        S.error("Your registration token has expired. Please re-register your email address to get a new token.")
        redirectTo(AppSiteMap.Register1)

      case _ => () // valid
    }

  def onSubmit(): JsCmd =
    try {
      import UserRegistrationResult._

      handleCompositeInvalidity(form validate vars)(r => {
        val (name, username, password, newsletter, _) = r
        val ps = PasswordAndSalt.createWithRandomSalt(password)

        val dbPlan = DbLogic.user.performRegistration(token)(username, ps, clientIp().getOrElse("?"))(name, newsletter)
        db().io.trans(dbPlan).unsafePerformIO() match {

          case UsernameTaken =>
            jsShowError("Username is already taken.")

          case NoMatchingConfToken =>
            S.error("Your registration token disappeared.")
            redirectTo(AppSiteMap.Login)

          // Registration complete
          case DbSuccess(id) =>
            log.info(s"Registered new user: $username")
            taskman().submitMsg(Msg.RegistrationCompleted(id)).unsafePerformIO()
            SecurityUtils.getSubject.login(new UsernamePasswordToken(username.value, password))
            JqExpr("#regComplete,#register2") ~> JqToggle
        }
      })
    } finally
      vars = vars put3 FormVar.emptyPasswordPair // Let's not keep the plaintext passwords around
}
