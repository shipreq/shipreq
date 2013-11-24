package com.beardedlogic.usecase.snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import com.beardedlogic.usecase.app.AppSiteMap
import com.beardedlogic.usecase.lib.SnippetHelpers
import com.beardedlogic.usecase.lib.Types.UserId
import com.beardedlogic.usecase.security.PasswordAndSalt

/**
 * Allows user to view and modify their account details.
 */
object UserAccount extends SnippetHelpers {

  def render = {
    val u = currentUser_!
    val uu = daoProvider.withSession(_ findUserSupplementalInfo u) getOrElse redirectTo(AppSiteMap.Logout)
    (
      ".username .form-control-static *" #> u.username
      & ".email .form-control-static *" #> u.email
      & ".registeredAt time [datetime]" #> uu.registeredAt
      & ".password .edit" #> DynModal.passwordChangerT("Account Password", Some(uu.ps))(onPasswordChange(u))
    )
  }

  def onPasswordChange(id: UserId)(newPassword: PasswordAndSalt): JsCmd = {
    daoProvider.withSession(_.updateUserPassword(id, newPassword))
    jsShowNotice("Updated Account Password.")
  }
}
