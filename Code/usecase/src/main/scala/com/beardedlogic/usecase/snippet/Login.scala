package com.beardedlogic.usecase
package snippet

import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc._
import scala.xml.Text
import scalaz.{-\/,\/-}
import lib.{InputValidator, SingleOpStatefulSnippet}
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
    val possibleJs = for {
      usernameOrEmail <- InputValidator.usernameOrEmail.correctAndValidate(usernameOrEmailInput)
      password        <- InputValidator.password.correctAndValidate(passwordInput)
    } yield {
      val loginToken = new UsernamePasswordToken(usernameOrEmail, password)
      loginToken.setRememberMe(false)
      try {
        SecurityUtils.getSubject.login(loginToken)
        onSuccessfulLogin()
      } catch {
        case _: AuthenticationException => jsShowError(InvalidLogin)
      }
    }
    possibleJs | jsShowError(InvalidLogin)
  }

  def onSuccessfulLogin(): Nothing = {
    // TODO update login count async
    daoProvider.withSession(_.logUserLogin(currentUserId_!, clientIp_Or_?))
    redirectHome
  }
}
