package com.beardedlogic.usecase
package snippet

import net.liftweb.util.Helpers._
import scalaz.NonEmptyList.nel

import app.{RequestVars, AppSiteMap}
import AppSiteMap.Implicits._
import lib.Types._
import lib.SingleOpStatefulSnippet
import db.ShareSummary
import feature.UcFilter
import util.ConciseIntListDesc
import net.liftweb.util.CssSel

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

  def renderShare(s: ShareSummary) = (
    ".name a *" #> s.name
    & ".name a [href]" #> urlFor(s)
    & ".url :text [value]" #> urlFor(s)
    & ".url .copy [data-clipboard-text]" #> urlFor(s)
    & ".ucdesc *" #> descMatchingUcs(UcFilter.fromJson(s.ucFilterJson))
    & ".views .v" #> descViewCount(s.viewCount)
    & ".views .r" #> renderViewRecency(s.lastViewedAt)
  )

  def descMatchingUcs(f: UcFilter): String = {
    val m = UcFilter.apply(f, ucs)
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

  def urlFor(s: ShareSummary): String = "http://blahblahblah/" + s.urlToken
}
