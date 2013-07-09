package com.beardedlogic.usecase
package snippet

import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Helpers._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc._
import scala.xml.Text
import app.AppSiteMap
import com.beardedlogic.usecase.lib.{InputCorrection, SingleOpStatefulSnippet}
import util.Reactor
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
    ":submit" #> ajaxSubmitOnClick(jsCallback(onLoginAttempt(_)))
    )

  def onLoginAttempt(implicit reactor: Reactor) {
    if (usernameOrEmail.isEmpty || password.isEmpty)
      reactWithError(InvalidLogin)
    else {
      val loginToken = new UsernamePasswordToken(usernameOrEmail, password)
      loginToken.setRememberMe(rememberMe)
      try {
        SecurityUtils.getSubject.login(loginToken)
        onSuccessfulLogin()
      } catch {
        case _: AuthenticationException => reactWithError(InvalidLogin)
      }
    }
  }

  def onSuccessfulLogin() {
    // TODO update login count async
    daoProvider.withSession(_.updateUserOnLogin(loggedInUser.get.id, clientIp_Or_?))
    S.redirectTo(AppSiteMap.HomeRelativeUrl)
  }
}
