package com.beardedlogic.usecase.snippet

import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import com.beardedlogic.usecase.app.AppSiteMap
import com.beardedlogic.usecase.feature.{UcFilters, UcFilter}
import com.beardedlogic.usecase.feature.validation.Validator
import com.beardedlogic.usecase.lib.{NoticeFlash, SingleOpStatefulSnippet}
import com.beardedlogic.usecase.lib.Types.ProjectId
import com.beardedlogic.usecase.security.PasswordAndSalt
import com.beardedlogic.usecase.util.HtmlTransformExt.ajaxSubmitOnClick
import project.ActivateTab

/**
 * Allows a user to create a new share.
 *
 * @since 30/10/2013
 */
class ShareCreate(projectId: ProjectId) extends SingleOpStatefulSnippet {

  var nameInput = ""
  var password1Input = ""
  var password2Input = ""
  var prefaceInput = ""

  def render = {
    val ucs = daoProvider.withSession(_.findAllLatestUseCaseRevsByProject(projectId))
    val (ucFilterXml, ucFilterFn) = UcFilter.render(UcFilters.All, ucs)

    (
      "#shareName" #> SHtml.onSubmit(nameInput = _)
      & "#password1" #> SHtml.onSubmit(password1Input = _)
      & "#password2" #> SHtml.onSubmit(password2Input = _)
      & "#preface" #> SHtml.onSubmit(prefaceInput = _)
      & "#uc-filters" #> ucFilterXml
      & ":submit" #> ajaxSubmitOnClick(() => onSubmit(ucFilterFn))
    )
  }

  def onSubmit(ucFilterFn: () => UcFilter): JsCmd = try {
    val v = Validator.Ap.apply3(
      Validator.shareName.correctAndValidate(nameInput),
      Validator.passwords.correctAndValidate(password1Input, password2Input),
      Validator.sharePreface.correctAndValidate(prefaceInput)
    )(Tuple3.apply)

    ifValid(v)(r => {
      val (name, password, prefaceT) = r
      val ucFilter = ucFilterFn()
      val ps = PasswordAndSalt.createWithRandomSalt(password)
      val preface = nonEmptyString(prefaceT)
      val ucFilterJson = UcFilter.toJson(ucFilter)
      daoProvider.withSession(_.createShare(projectId, ps, name, preface, ucFilterJson))
      postCreation()
    })

  } finally {
    password1Input = "" // Let's not keep the plaintext passwords around
    password2Input = ""
  }

  def postCreation(): Nothing = {
    NoticeFlash.notices.addS("Share created successfully.")
    ActivateTab.SharesTab.setInFlash()
    redirectTo(AppSiteMap.Project)(projectId)
  }
}
