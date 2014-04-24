package shipreq.webapp.snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import shipreq.webapp.app.AppSiteMap
import shipreq.webapp.feature.{UcFilters, UcFilter}
import shipreq.webapp.feature.validation.Validators
import shipreq.webapp.lib.{FormVar, NoticeFlash, SingleOpStatefulSnippet}
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

  val nameV    = FormVar.strOnSubmit(Validators.share.name, "#shareName")("")
  val prefaceV = FormVar.strOnSubmit(Validators.share.preface, "#preface")("")

  protected def render2(f: UcFilter) = {
    val ucs = daoProvider.withSession(_ findAllLatestUseCaseRevsByProject projectId)
    val (ucFilterXml, ucFilterFn) = UcFilter.render(f, ucs)
    def readHttpParamsAndBuildUcFilterJson(): Json[UcFilter] = UcFilter.toJson(ucFilterFn())
    (
      nameV.csssel
      & prefaceV.csssel
      & "#uc-filters" #> ucFilterXml
      & ":submit" #> ajaxSubmitOnClick(() => onSubmit(readHttpParamsAndBuildUcFilterJson))
    )
  }

  def onSubmit(ucFilterJson: () => Json[UcFilter]): JsCmd

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

  val passwordV = FormVar.passwordPair("#password1", "#password2")

  def render =
    render2(UcFilters.All) & passwordV.csssel

  def onSubmit(ucFilterJson: () => Json[UcFilter]): JsCmd = {
    val v = try
      FormVar.AP3(nameV, passwordV, prefaceV).validate(Tuple3.apply)
    finally
      passwordV.fv set2 "" // Let's not keep the plaintext passwords around

    ifValid(v)(r => {
      val (name, password, preface) = r
      val ps = PasswordAndSalt.createWithRandomSalt(password)
      daoProvider.withSession(_.createShare(projectId, ps, name, preface, ucFilterJson()))
      NoticeFlash.notices.addS(s"Created Share: $name")
      goBackToShareList()
    })
  }
}
