package shipreq.webapp.snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import shipreq.taskman.api.Msg.UserUpdated
import shipreq.webapp.app.AppSiteMap
import shipreq.webapp.db.UserDetail
import shipreq.webapp.feature.validation.Validators
import shipreq.webapp.lib.{FormVar, SnippetHelpers}
import shipreq.webapp.lib.Types._
import shipreq.webapp.security.PasswordAndSalt
import shipreq.webapp.util.HtmlTransformExt._

object UserAccount {
  val form = FormVar.merge(
    FormVar.strOnSubmit(Validators.landingPage.name, "#usrname"),
    FormVar.boolOnSubmit("#newsletter")
  )(UserDetail)
}

/**
 * Allows user to view and modify their account details.
 */
class UserAccount extends SnippetHelpers {
  import UserAccount.form

  val usr = currentUser_!
  val (supp, usrd) = daoProvider.withSession(_ findUserSuppAndDetail usr) getOrElse redirectTo(AppSiteMap.Logout)
  var vars: form.Var = (usrd.name, usrd.newsletter)

  def render = (
    ".username .form-control-static *" #> usr.username
    & ".email .form-control-static *" #> usr.email
    & ".registeredAt time [datetime]" #> supp.registeredAt.value
    & ".password .edit" #> DynModal.passwordChangerT("Account Password", Some(supp.ps))(onPasswordChange)
    & form.csssel(vars, vars = _)
    & "#usrd-submit" #> ajaxSubmitOnClick(onUserPrefUpdate)
  )

  def onPasswordChange(newPassword: PasswordAndSalt): JsCmd = {
    daoProvider.withSession(_.updateUserPassword(usr, newPassword))
    jsShowNotice("Updated Account Password.")
  }

  def onUserPrefUpdate(): JsCmd =
    ifValid(form.validate(vars))(d => {
      daoProvider.withTransaction(dao => {
        dao.updateUserDetails(usr, d)
        taskmanD(dao, _ submitMsg UserUpdated(usr))
      })
      jsShowNotice("Updated user details.", "usrdupd")
    })
}
