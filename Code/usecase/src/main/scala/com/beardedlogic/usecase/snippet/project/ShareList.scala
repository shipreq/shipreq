package com.beardedlogic.usecase
package snippet.project

import net.liftweb.http.js._
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._

import app.{RequestVars, AppSiteMap}
import db.ShareSummary
import feature.UcFilter
import lib.ScalazSubset._
import lib.SingleOpStatefulSnippet
import lib.Types._
import security.PasswordAndSalt
import snippet.DynModal
import util.{NonEmptyTemplate, ConciseIntListDesc}
import util.HtmlTransformExt.ajaxOnClick
import util.JsExt._
import AppSiteMap.Implicits._
import ShareListConsts._

object ShareListConsts {
  val DeleteModalBody = NonEmptyTemplate.load("templates-hidden/modal_body-delete_share").get
}

/**
 * Displays a list of a user's shares.
 *
 * @since 30/10/2013
 */
class ShareList(projectId: ProjectId) extends SingleOpStatefulSnippet {

  val ucs = RequestVars.UseCases.get

  def render = {
    val shares = daoProvider.withSession(_.summariseShares(projectId))
    renderCreateButton & renderShares(shares)
  }

  def renderCreateButton =
    ".create a [href]" #> AppSiteMap.ShareCreate.relativeUrl(projectId)

  def renderShares(shares: List[ShareSummary]) =
    if (shares.isEmpty)
      "#share-list" #> ""
    else
      "#share-list li .share" #> shares.map(renderShare)

  def renderShare(s: ShareSummary) = {
    val absUrl = AppSiteMap.ShareView.absoluteUrl(s.urlToken)
    val relUrl = AppSiteMap.ShareView.relativeUrl(s.urlToken)
    (
      "li [id+]" #> shareLiId(s)
      & ".l" #> (
        ".edit [href]" #> AppSiteMap.ShareEdit.relativeUrl(s.urlToken)
        & ".chgpwd" #> ajaxOnClick(() => DynModal.passwordChanger("Change Share Password")(onPasswordChange(s)))
        & ".delete" #> ajaxOnClick(() => DynModal.confirmDanger(Some(s.name), DeleteModalBody, "Delete Share")(onDelete(s)))
      )
      & ".r" #> (
        ".name a *" #> s.name
        & ".name a [href]" #> relUrl
        & ".url :text [value]" #> absUrl
        & ".url .copy [data-clipboard-text]" #> absUrl
        & ".ucdesc *" #> descMatchingUcs(s.ucFilter)
        & ".views .v" #> descViewCount(s.viewCount)
        & ".views .r" #> renderViewRecency(s.lastViewedAt)
      )
    )
  }

  def shareLiId(s: ShareSummary): String = "s-" + s.urlToken

  def descMatchingUcs(f: UcFilter): String = {
    val m = UcFilter.apply(f)(ucs)
    val msize = m.size
    msize match {
      case 0 => "0 use cases."
      case 1 => s"1 use case: ${m.head.number}."
      case _ =>
        val mnel = nel(m.head, m.tail)
        val idDesc = ConciseIntListDesc.compute(mnel)(_.number.toInt)
        s"$msize use cases: $idDesc."
    }
  }

  def descViewCount(viewCount: Long): String =
    if (viewCount == 1)
      "1 view."
    else
      s"$viewCount views."

  def renderViewRecency(lastViewedAt: Option[String @@ ISO8601]): CssSel = lastViewedAt match {
    case None => "*" #> ""
    case Some(at) => "abbr [title]" #> at
  }

  def onPasswordChange(s: ShareId)(newPassword: String @@ Validated): JsCmd = {
    val ps = PasswordAndSalt.createWithRandomSalt(newPassword)
    daoProvider.withSession(_.updateSharePassword(s, ps))
    jsShowNotice("Password updated successfully.")
  }

  def onDelete(s: ShareSummary): JsCmd = {
    daoProvider.withSession(_ deleteShare s)
    FadeOutThen(JqId(shareLiId(s)), Slow)(_ ~> JqRemove & jsShowNotice("Share deleted successfully."))
  }
}
