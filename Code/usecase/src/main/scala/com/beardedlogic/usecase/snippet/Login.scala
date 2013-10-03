package com.beardedlogic.usecase
package snippet

import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc._
import scala.xml.Text
import lib.{InputCorrection, SingleOpStatefulSnippet}
import util.HtmlTransformExt.ajaxSubmitOnClick
import Login._

object Login {
  final val InvalidLogin = Text("Invalid login details.")
}

class Login extends SingleOpStatefulSnippet {

  private var usernameOrEmail, password = ""
  private var rememberMe = true

  // TODO What about when user already logged in?

  def render = (
    "#who" #> SHtml.onSubmit(i => usernameOrEmail = InputCorrection.usernameOrEmail(i)) &
    "#password" #> SHtml.onSubmit(i => password = InputCorrection.password(i)) &
    "#remember" #> SHtml.checkbox(rememberMe, rememberMe = _, "id" -> "remember") &
    ":submit" #> ajaxSubmitOnClick(onLoginAttempt)
    )

  def onLoginAttempt(): JsCmd = {
    if (usernameOrEmail.isEmpty || password.isEmpty)
      jsShowError(InvalidLogin)
    else {
      val loginToken = new UsernamePasswordToken(usernameOrEmail, password)
      loginToken.setRememberMe(rememberMe)
      try {
        SecurityUtils.getSubject.login(loginToken)
        onSuccessfulLogin()
      } catch {
        case _: AuthenticationException => jsShowError(InvalidLogin)
      }
    }
  }

  def onSuccessfulLogin(): Nothing = {
    // TODO update login count async
    daoProvider.withSession(_.logUserLogin(currentUserId_!, clientIp_Or_?))
    redirectHome
  }
}
