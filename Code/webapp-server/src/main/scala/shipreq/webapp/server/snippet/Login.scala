package shipreq.webapp.server.snippet

import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc._
import scala.xml.Text
import scalaz.{-\/, \/-}
import shipreq.webapp.server.lib.{FormVar, LogUserLogin, SingleOpStatefulSnippet}
import shipreq.webapp.server.feature.validation.ServerSideValidators
import shipreq.webapp.server.util.HtmlTransformExt.ajaxSubmitOnClick
import Login._

object Login {
  val invalidLogin = Text("Invalid login details.")

  val form = FormVar.merge(
    FormVar.strOnSubmit(ServerSideValidators.user.usernameOrEmail, "#who"),
    FormVar.strOnSubmit(ServerSideValidators.password.named, "#password")
  )(new UsernamePasswordToken(_, _))
}

class Login extends SingleOpStatefulSnippet {

  // TODO What about when user already logged in?

  private var vars: form.Var = ("", "")

  def render =
    form.csssel(vars, vars = _) & ":submit" #> ajaxSubmitOnClick(() => onLoginAttempt())

  def onLoginAttempt(): JsCmd = {
    securityProvider().enforceHumanSpeed()
    form.validate(vars) match {
      case \/-(loginToken) =>
        loginToken.setRememberMe(false)
        try {
          SecurityUtils.getSubject.login(loginToken)
          onSuccessfulLogin()
        } catch {
          case _: AuthenticationException => jsShowError(invalidLogin)
        }
      case -\/(f) =>
        jsShowError(invalidLogin)
    }
  }

  def onSuccessfulLogin(): Nothing = {
    statLogger().updateSessionStatsOnLogin(S.session, currentUser_!())
    statLogger() ! LogUserLogin(currentUserId_!())
    redirectHome()
  }
}
