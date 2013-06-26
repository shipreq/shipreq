package com.beardedlogic.usecase.snippet

import net.liftweb.http.{S, StatefulSnippet, SHtml}
import net.liftweb.util.Helpers._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc._

class Login extends StatefulSnippet {
  override def dispatch = { case "render" => render }

  private var usernameOrEmail, password = ""
  private var rememberMe = true

  def render = {
    "#who" #> SHtml.onSubmit(i => usernameOrEmail = i.trim) &
      "#who [value]" #> usernameOrEmail &
      "#password" #> SHtml.onSubmit(password = _) &
      "#remember" #> SHtml.checkbox(rememberMe, rememberMe = _, "id" -> "remember") &
      "type=submit" #> SHtml.onSubmitUnit(onLoginAttempt)
  }

  def onLoginAttempt() {
    // TODO Check password length when range constraints implemented
    if (usernameOrEmail.isEmpty || password.isEmpty)
      S.error("Invalid login details.")
    else {
      val subject = SecurityUtils.getSubject
      val loginToken = new UsernamePasswordToken(usernameOrEmail, password)
      loginToken.setRememberMe(rememberMe)
      try {
        subject.login(loginToken)
        onSuccessfulLogin()
      } catch {
        // case _: LockedAccountException =>
        // case _: ExcessiveAttemptsException =>
        case _: AuthenticationException => S.error("Invalid login details.")
      }
    }
  }

  def onSuccessfulLogin(): Unit = S.redirectTo("/")
}
