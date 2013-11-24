package com.beardedlogic.usecase.snippet

import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import com.beardedlogic.usecase.app.AppSiteMap
import com.beardedlogic.usecase.feature.{UcFilters, UcFilter}
import com.beardedlogic.usecase.feature.validation.Validator
import com.beardedlogic.usecase.lib.{NoticeFlash, SingleOpStatefulSnippet}
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.security.PasswordAndSalt
import com.beardedlogic.usecase.util.HtmlTransformExt.ajaxSubmitOnClick
import project.ActivateTab

/**
 * Shared between ShareCreate and ShareEdit.
 * Both display a similar form and perform similar validations.
 */
private[snippet] abstract class ShareCreateBase extends SingleOpStatefulSnippet {

  def projectId: ProjectId

  var nameInput = ""
  var prefaceInput = ""

  protected def render2(f: UcFilter) = {
    val ucs = daoProvider.withSession(_ findAllLatestUseCaseRevsByProject projectId)
    val (ucFilterXml, ucFilterFn) = UcFilter.render(f, ucs)
    def readHttpParamsAndBuildUcFilterJson(): Json[UcFilter] = UcFilter.toJson(ucFilterFn())
    (
      "#shareName" #> SHtml.onSubmit(nameInput = _)
      & "#preface" #> SHtml.onSubmit(prefaceInput = _)
      & "#uc-filters" #> ucFilterXml
      & ":submit" #> ajaxSubmitOnClick(() => onSubmit(readHttpParamsAndBuildUcFilterJson))
    )
  }

  def onSubmit(ucFilterJson: () => Json[UcFilter]): JsCmd

  def nameV = Validator.shareName.correctAndValidate(nameInput)
  def prefaceV = Validator.sharePreface.correctAndValidate(prefaceInput)

  def goBackToShareList(): Nothing = {
    ActivateTab.SharesTab.setInFlash()
    redirectTo(AppSiteMap.Project)(projectId)
  }
}

/**
 * Allows a user to create a new share.
 *
 * @since 30/10/2013
 */
class ShareCreate(val projectId: ProjectId) extends ShareCreateBase {

  var password1Input = ""
  var password2Input = ""

  def render = (
    render2(UcFilters.All)
      & "#password1" #> SHtml.onSubmit(password1Input = _)
      & "#password2" #> SHtml.onSubmit(password2Input = _)
    )

  def onSubmit(ucFilterJson: () => Json[UcFilter]): JsCmd = {
    val v = try
      Validator.Ap.apply3(nameV, Validator.passwords.correctAndValidate(password1Input, password2Input), prefaceV)(Tuple3.apply)
    finally {
      password1Input = "" // Let's not keep the plaintext passwords around
      password2Input = ""
    }

    ifValid(v)(r => {
      val (name, password, preface) = r
      val ps = PasswordAndSalt.createWithRandomSalt(password)
      daoProvider.withSession(_.createShare(projectId, ps, name, preface, ucFilterJson()))
      NoticeFlash.notices.addS(s"Created Share: $name")
      goBackToShareList()
    })
  }
}
