package com.beardedlogic.usecase
package snippet

import net.liftweb.http.js.JsCmds
import net.liftweb.http.{StatefulSnippet, SHtml}
import net.liftweb.util.Helpers._
import net.liftweb.util.Mailer._

import lib.JsExt._
import lib.SnippetHelpers
import lib.msg.{JavaScript, Reactor}
import mail.RegistrationEmails
import model.{DAO, UserRegistrationInfo}

/**
 * Takes an email address, validates it, creates a new user, sends an email with a verification-token in it.
 *
 * @since 27/06/2013
 */
class Register1 extends StatefulSnippet with SnippetHelpers {
  override def dispatch = { case _ => render }

  var emailInput = ""

  def render = (
    "#email" #> SHtml.text(emailInput, emailInput = _, "id" -> "email")
      // & "form *+" #> SHtml.hidden(jsCallback(onSubmit(_)))
      & ":submit" #> SHtml.ajaxSubmit("Create Account", jsCallback(onSubmit(_)))
    )

  def onSubmit(implicit reactor: Reactor) {
    val email = normaliseEmail(emailInput)
    if (!isEmailValid_?(email)) {
      error("Please enter a valid email address.")
    } else {
      val mail: Mail = daoProvider.withTransaction(dao =>
        dao.findUserRegistrationInfo(email) match {
          case None => onNewUser(email, dao)
          case Some(UserRegistrationInfo(_, _, _, Some(_))) => onAlreadyRegistered()
          case Some(UserRegistrationInfo(_, Some(token), Some(issued), _)) if !isConfirmationTokenExpired_?(issued) => onTokenReusable(token)
          case Some(UserRegistrationInfo(id, _, _, _)) => onTokenExpired(id, dao)
        }
      )
      reactor(JavaScript)(JqExpr("#emailSent,#register1Form") ~> JqToggle)
      sendMail(mail, To(email))
    }
  }

  private def error(errMsg: String)(implicit reactor: Reactor) {
    reactor(JavaScript)(JsCmds.Alert(errMsg))
  }

  private def onNewUser(email: String, dao: DAO): Mail = {
    val token = randomConfirmationToken
    dao.createUser(email, token)
    RegistrationEmails.LinkToCompleteRegistration(token)
  }

  private def onTokenReusable(token: String): Mail = {
    RegistrationEmails.LinkToCompleteRegistration(token)
  }

  private def onTokenExpired(id: Long, dao: DAO): Mail = {
    val token = randomConfirmationToken
    dao.updateUserConfirmationToken(id, token)
    RegistrationEmails.LinkToCompleteRegistration(token)
  }

  private def onAlreadyRegistered() = RegistrationEmails.AlreadyRegistered
}
