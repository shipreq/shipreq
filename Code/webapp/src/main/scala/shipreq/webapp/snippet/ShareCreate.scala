package shipreq.webapp.snippet

import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import shipreq.webapp.app.AppSiteMap
import shipreq.webapp.feature.{UcFilters, UcFilter}
import shipreq.webapp.feature.validation.Validator
import shipreq.webapp.lib.{NoticeFlash, SingleOpStatefulSnippet}
import shipreq.webapp.lib.Types._
import shipreq.webapp.security.PasswordAndSalt
import shipreq.webapp.util.HtmlTransformExt.ajaxSubmitOnClick
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

  def nameV = Validator.share.name.correctAndValidate(nameInput)
  def prefaceV = Validator.share.preface.correctAndValidate(prefaceInput)

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
