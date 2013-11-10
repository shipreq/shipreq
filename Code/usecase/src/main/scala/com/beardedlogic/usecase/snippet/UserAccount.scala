package com.beardedlogic.usecase.snippet

import net.liftweb.util.Helpers._
import com.beardedlogic.usecase.lib.SnippetHelpers
import com.beardedlogic.usecase.app.AppSiteMap

/**
 * Allows user to view and modify their account details.
 */
object UserAccount extends SnippetHelpers {

  def render = {
    val u = currentUser_!
    val uu = daoProvider.withSession(_ findUserSupplementalInfo u) getOrElse redirectTo(AppSiteMap.Logout)
    (
      ".username *" #> u.username
      & ".email *" #> u.email
      & ".registeredAt time [datetime]" #> uu.registeredAt
    )
  }
}
