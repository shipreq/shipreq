package com.beardedlogic.shipreq.snippet

import com.beardedlogic.shipreq.app.AppConfig
import com.beardedlogic.shipreq.app.AppSiteMap
import com.beardedlogic.shipreq.app.AppSiteMap.Implicits._
import com.beardedlogic.shipreq.lib.{Misc, SnippetHelpers}
import net.liftweb.http.DispatchSnippet
import net.liftweb.sitemap.{Loc, SiteMap}
import net.liftweb.util.Props
import net.liftweb.util.Props.RunModes.{Test, Development}
import scala.xml.{Elem, NodeSeq, Text}

/**
 * Creates a link to a page. Throws an error is the page is not found.
 */
object Link extends DispatchSnippet with SnippetHelpers {

  private type R = NodeSeq => NodeSeq

  override def dispatch = {
    case "App"    => appLink
    case "jquery" => jqueryLink
    case name     => ToPage(name)
  }

  private def staticLink(link: NodeSeq): R = _ => link

  val appLink =
    staticLink(<a href={AppSiteMap.Home.absoluteUrl}>{AppConfig.AppName}</a>)

  val jqueryLink = {
    val jqueryUrl = Props.mode match {
      case Development | Test => "/assets/vendor/jquery.js"
      case _                  => s"//ajax.googleapis.com/ajax/libs/jquery/${AppConfig.jQueryVersion}/jquery.min.js"
    }
    staticLink(<script src={jqueryUrl} type="text/javascript"></script>)
  }

  object ToPage {
    def apply(name: String): R = {
      val loc = SiteMap.findLoc(name) openOrThrowException s"Unable to generate link to $name"
      pageLinkMemo(loc)
    }

    private val pageLinkMemo =
      Misc.newMemo[Loc[_], R](Equiv.reference)(generatePageLink)

    private def generatePageLink(loc: Loc[_]): R = {
        val linkText = loc.linkText openOr Text(loc.name)
        val link = <a href={loc.relativeUrl}>{linkText}</a>
        n => n match {
          case <a>{customTitle}</a> => <a href={loc.relativeUrl}>{customTitle}</a>
          case _                    => link
        }
      }
  }
}
