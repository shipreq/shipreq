package shipreq.webapp.snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.TaggedTypes.JsonStr
import shipreq.webapp.app.AppSiteMap
import shipreq.webapp.feature.{UcFilters, UcFilter}
import shipreq.webapp.feature.validation.Validators
import shipreq.webapp.lib.{FormVar, NoticeFlash, SingleOpStatefulSnippet}
import shipreq.webapp.lib.Types._
import shipreq.webapp.security.PasswordAndSalt
import shipreq.webapp.util.HtmlTransformExt.ajaxSubmitOnClick
import project.ActivateTab
import ShareCreateBase._

private[snippet] object ShareCreateBase {
  val nameFV    = FormVar.strOnSubmit(Validators.share.name, "#shareName")
  val prefaceFV = FormVar.strOnSubmit(Validators.share.preface, "#preface")
}

/**
 * Shared between ShareCreate and ShareEdit.
 * Both display a similar form and perform similar validations.
 */
private[snippet] abstract class ShareCreateBase extends SingleOpStatefulSnippet {

  def projectId: ProjectId

  protected def render2(f: UcFilter) = {
    val ucs = daoProvider.withSession(_ findAllLatestUseCaseRevsByProject projectId)
    val (ucFilterXml, ucFilterFn) = UcFilter.render(f, ucs)
    def readHttpParamsAndBuildUcFilterJson(): JsonStr[UcFilter] = UcFilter.toJsonStr(ucFilterFn())

    ( "#uc-filters" #> ucFilterXml
    & ":submit" #> ajaxSubmitOnClick(() => onSubmit(readHttpParamsAndBuildUcFilterJson))
    )
  }

  def onSubmit(ucFilterJson: () => JsonStr[UcFilter]): JsCmd

  def goBackToShareList(): Nothing = {
    ActivateTab.SharesTab.setInFlash()
    redirectTo(AppSiteMap.Project)(projectId)
  }
}

object ShareCreate {
  val createForm = FormVar.merge(nameFV, prefaceFV, FormVar.passwordPair("#password1", "#password2"))(Tuple3.apply)
}

/**
 * Allows a user to create a new share.
 *
 * @since 30/10/2013
 */
class ShareCreate(val projectId: ProjectId) extends ShareCreateBase {
  import ShareCreate._

  var vars: createForm.Var = ("", "", FormVar.emptyPasswordPair)

  def render =
    render2(UcFilters.All) & createForm.csssel(vars, vars = _)

  def onSubmit(ucFilterJson: () => JsonStr[UcFilter]): JsCmd = {
    val v = try
      createForm validate vars
    finally
      vars = vars put3 FormVar.emptyPasswordPair // Let's not keep the plaintext passwords around
    ifValid(v)(t => {
      val (name, preface, password) = t
      val ps = PasswordAndSalt.createWithRandomSalt(password)
      daoProvider.withSession(_.createShare(projectId, ps, name, preface, ucFilterJson()))
      NoticeFlash.notices.addS(s"Created Share: $name")
      goBackToShareList()
    })
  }
}
