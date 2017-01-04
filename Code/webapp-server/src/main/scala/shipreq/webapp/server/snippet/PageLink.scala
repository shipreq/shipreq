package shipreq.webapp.server.snippet

import net.liftweb.http.DispatchSnippet
import net.liftweb.sitemap.{Loc, SiteMap}
import scala.xml._
import shipreq.base.util.Memo
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.server.app.AppSiteMap
import shipreq.webapp.server.app.AppSiteMap.Implicits._
import shipreq.webapp.server.lib.SnippetHelpers

/**
 * Creates a link to a page. Throws an error is the page is not found.
 */
object PageLink extends DispatchSnippet with SnippetHelpers {

  private type R = NodeSeq => NodeSeq

  override def dispatch = {
    case "App" => appLink
    case name  => toPage(name)
  }

  val appLink =
    staticHtml(<a href={AppSiteMap.Home.absoluteUrl}>{WebappConfig.appName}</a>)

  private def generatePageLink(loc: Loc[_]): R = {
    val linkText = loc.linkText openOr Text(loc.name)
    val a = new UnprefixedAttribute("href", loc.relativeUrl, Null)

    n => n.iterator.take(2).toList match {
      case (e: Elem) :: Nil =>
        e.copy(
          attributes = e.attributes.remove("data-lift").append(a),
          child = if (e.child.nonEmpty) e.child else linkText.theSeq)
      case x :: Nil =>
        sys error s"Don't know how to turn ${x.getClass} $x into a PageLink."
      case _ =>
        sys error s"PageLink can only be applied to a single node. Input ${n.theSeq} is not applicable."
    }
  }

  private val pageLinkMemo =
    Memo.byRef[Loc[_], R](generatePageLink)

  def toPage(name: String) = {
    val loc = SiteMap.findLoc(name) openOrThrowException s"No page found in sitemap called '$name'"
    pageLinkMemo(loc)
  }
}
