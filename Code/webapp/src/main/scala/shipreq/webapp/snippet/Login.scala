package shipreq.webapp
package snippet

import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc._
import scala.xml.Text
import scalaz.{Failure, Success}
import lib.{LogUserLogin, SingleOpStatefulSnippet}
import feature.validation.Validator
import util.HtmlTransformExt.ajaxSubmitOnClick
import Login._

object Login {
  final val InvalidLogin = Text("Invalid login details.")
}

class Login extends SingleOpStatefulSnippet {

  private var usernameOrEmailInput, passwordInput = ""

  // TODO What about when user already logged in?

  def render = (
    "#who" #> SHtml.onSubmit(usernameOrEmailInput = _) &
    "#password" #> SHtml.onSubmit(passwordInput = _) &
    ":submit" #> ajaxSubmitOnClick(onLoginAttempt)
  )

  def onLoginAttempt(): JsCmd = {
    securityProvider.enforceHumanSpeed()

    val v = Validator.Ap.apply2(
      Validator.user.usernameOrEmail.correctAndValidate(usernameOrEmailInput),
      Validator.password.correctAndValidate(passwordInput)
    )(new UsernamePasswordToken(_, _))

    v match {
      case Success(loginToken) =>
        loginToken.setRememberMe(false)
        try {
          SecurityUtils.getSubject.login(loginToken)
          onSuccessfulLogin()
        } catch {
          case _: AuthenticationException => jsShowError(InvalidLogin)
        }
      case Failure(f) =>
        jsShowError(InvalidLogin)
    }
  }

  def onSuccessfulLogin(): Nothing = {
    statLogger.updateSessionStatsOnLogin(S.session, currentUser_!)
    statLogger ! LogUserLogin(currentUserId_!)
    redirectHome
  }
}
