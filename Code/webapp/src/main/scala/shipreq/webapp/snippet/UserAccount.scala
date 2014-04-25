package shipreq.webapp.snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import shipreq.webapp.app.AppSiteMap
import shipreq.webapp.lib.SnippetHelpers
import shipreq.webapp.lib.Types._
import shipreq.webapp.security.PasswordAndSalt

/**
 * Allows user to view and modify their account details.
 */
object UserAccount extends SnippetHelpers {

  def render = {
    val usr = currentUser_!
    val (supp, usrd) = daoProvider.withSession(_ findUserSuppAndDetail usr) getOrElse redirectTo(AppSiteMap.Logout)
    (
      ".username .form-control-static *" #> usr.username
      & ".email .form-control-static *" #> usr.email
      & ".registeredAt time [datetime]" #> supp.registeredAt
      & ".password .edit" #> DynModal.passwordChangerT("Account Password", Some(supp.ps))(onPasswordChange(usr))
      & ".name .form-control-static *" #> usrd.name
      & ".newsletter .form-control-static *" #> (if (usrd.newsletter) "Subscribed" else "Not subscribed")
    )
  }

  def onPasswordChange(id: UserId)(newPassword: PasswordAndSalt): JsCmd = {
    daoProvider.withSession(_.updateUserPassword(id, newPassword))
    jsShowNotice("Updated Account Password.")
  }
}
