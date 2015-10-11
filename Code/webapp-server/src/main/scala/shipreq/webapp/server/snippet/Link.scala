package shipreq.webapp.server.snippet

import shipreq.webapp.server.app.AppConfig._
import shipreq.webapp.server.app.AppSiteMap
import shipreq.webapp.server.app.AppSiteMap.Implicits._
import shipreq.webapp.server.lib.{Misc, SnippetHelpers}
import shipreq.webapp.base.AppConsts
import net.liftweb.http.DispatchSnippet
import net.liftweb.sitemap.{Loc, SiteMap}
import net.liftweb.util.Props
import net.liftweb.util.Props.RunModes.{Test, Development}
import scala.xml.{Elem, Node, NodeSeq, Null, Text, UnprefixedAttribute}

/**
 * Creates a link to a page. Throws an error is the page is not found.
 */
object Link extends DispatchSnippet with SnippetHelpers {

  private type R = NodeSeq => NodeSeq

  override def dispatch = {
    case "App"      => appLink
    case "jquery"   => jqueryLink
    case "clientJs" => clientJs
    case "katex"    => katex
    case name       => ToPage(name)
  }

  private def static(link: NodeSeq): R = _ => link

  val appLink =
    static(<a href={AppSiteMap.Home.absoluteUrl}>{AppConsts.appName}</a>)

  private def sbtInReleaseMode =
    sys.props get "MODE" exists (_ == "release")

  private def useDevResources = (Props.mode, sbtInReleaseMode) match {
    case (Development, false) | (Test, false) => true
    case _ => false
  }

  val jqueryLink = {
    val jqueryUrl = Props.mode match {
      case Development | Test => s"$devAssetPath/jquery.js"
      case _                  => s"//ajax.googleapis.com/ajax/libs/jquery/$jQueryVersion/jquery.min.js"
    }
    static(<script type="text/javascript" src={jqueryUrl}></script>)
  }

  val clientJs = {
    val reactUrl = if (useDevResources) s"$devAssetPath/react.js" else s"$vendorAssetPath/react.js"
    val reactDomUrl = if (useDevResources) s"$devAssetPath/react-dom.js" else s"$vendorAssetPath/react-dom.js"
    val clientJsUrl = if (useDevResources) s"$devAssetPath/webapp-client-fastopt.js" else "/assets/C.js"
    static(
      <script type="text/javascript" src={reactUrl}></script>
      <script type="text/javascript" src={reactDomUrl}></script>
      <script type="text/javascript" src={clientJsUrl}></script>)
  }

  val katex = {
    val js  = s"$vendorAssetPath/katex/katex.min.js"
    val css = s"$vendorAssetPath/katex/katex.min.css"
    static(
      <script type="text/javascript" src={js}></script>
      <link data-lift="head" type="text/css" rel="stylesheet" href={css} />)
  }

  object ToPage {
    def apply(name: String): R = {
      val loc = SiteMap.findLoc(name) openOrThrowException s"No page found in sitemap called '$name'"
      pageLinkMemo(loc)
    }

    private val pageLinkMemo =
      Misc.newMemo[Loc[_], R](Equiv.reference)(generatePageLink)

    private def generatePageLink(loc: Loc[_]): R = {
      val linkText = loc.linkText openOr Text(loc.name)
      val a = new UnprefixedAttribute("href", loc.relativeUrl, Null)
      n => n match {
        case Elem(prefix, label, attrs, ns, ch@_*) =>
          val inner: Seq[Node] = if (ch.nonEmpty) ch else linkText.theSeq
          val newAttr = attrs.remove("data-lift").append(a)
          Elem(prefix, label, newAttr, ns, false, inner: _*)
      }
    }
  }
}
