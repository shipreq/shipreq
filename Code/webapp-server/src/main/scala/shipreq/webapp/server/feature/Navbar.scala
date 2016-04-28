package shipreq.webapp.server.feature

import scalaz.old.NonEmptyList
import scala.xml.{Attribute, Elem, NodeSeq, Null, Text}

import shipreq.webapp.server.app.{AppSiteMap, RequestVars}
import shipreq.webapp.server.lib.SnippetHelpers.shouldNeverHappen_!
import shipreq.webapp.server.lib.ScalazSubset._
import AppSiteMap.Implicits._

/**
 * @param elemsReversed Elements to appear in the Navbar. The visually-right-most item must be the first item in the
 *                      list, the left-most the last.
 */
case class Navbar(elemsReversed: NonEmptyList[NavbarElem])

sealed trait NavbarElem {
  def render: NodeSeq
  def renderActive: NodeSeq
  def customiseLi(li: Elem) = li
}

object Navbar {

  private def addClasses(e: Elem, classes: String): Elem = {
    val newClassAttr = e.attribute("class") match {
      case None           => classes
      case Some(existing) => existing.text + " " + classes
    }
    e % Attribute("class", Text(newClassAttr), Null)
  }

  private val DudLinkAttr =
    Attribute("onclick", Text("return !1"),
      Attribute("href", Text("#"),
        Null))

  sealed abstract class StaticLinkElem(link: Elem) extends NavbarElem {
    override def render = link
    override val renderActive = link % DudLinkAttr
  }

  case object Home extends StaticLinkElem(<a href={AppSiteMap.Home.relativeUrl}>Home</a>)

  case object CurrentProject extends NavbarElem {
    def project               = RequestVars.Project.get.value
    def link: Elem            = <a class="project">{project.name}</a>
    override def render       = link % Attribute("href", Text(AppSiteMap.Project.relativeUrl(project.id)), Null)
    override def renderActive = link % DudLinkAttr
  }

  case class StaticText(text: String) extends NavbarElem {
    override val render = <a>{text}</a> % DudLinkAttr
    override def renderActive = render
  }
}
