package com.beardedlogic.usecase
package snippet

import net.liftweb.http.{S, StatefulSnippet, SHtml}
import net.liftweb.util.Helpers._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc._
import lib.SnippetHelpers
import model.DAO

class Login extends StatefulSnippet with SnippetHelpers {
  override def dispatch = { case _ => render }

  private var usernameOrEmail, password = ""
  private var rememberMe = true

  // TODO What about when user already logged in?

  def render = (
    "#who" #> SHtml.onSubmit(i => usernameOrEmail = i.trim) &
      "#who [value]" #> usernameOrEmail &
      "#password" #> SHtml.onSubmit(password = _) &
      "#remember" #> SHtml.checkbox(rememberMe, rememberMe = _, "id" -> "remember") &
      "type=submit" #> SHtml.onSubmitUnit(onLoginAttempt)
    )

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

  def onSuccessfulLogin() {
    DAO.withSession(_.updateUserOnLogin(loggedInUser.get.id, clientIp.getOrElse("?")))
    S.redirectTo("/")
  }

  def clientIp: Option[String] = (
    S.originalRequest.map(_.remoteAddr)
      or S.containerRequest.map(_.remoteAddress)
      or S.request.map(_.remoteAddr)
    // println("X-Real-IP: " + req.header("X-Real-IP"))
    // println("X-Forwarded-For: " + req.header("X-Forwarded-For"))
    )
}
