package shipreq.webapp.feature

import scalaz.NonEmptyList
import scala.xml.{Attribute, Elem, NodeSeq, Null, Text}

import shipreq.webapp.app.{AppSiteMap, RequestVars}
import shipreq.webapp.db.UseCaseSummary
import shipreq.webapp.lib.SnippetHelpers.shouldNeverHappen_!
import shipreq.webapp.lib.ScalazSubset._
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
    override def render       = link % Attribute("href", Text(AppSiteMap.Project.relativeUrl(project)), Null)
    override def renderActive = link % DudLinkAttr
  }

  case object UseCaseDropdown extends NavbarElem {
    override def customiseLi(li: Elem) = addClasses(li, "dropdown ucs")
    override def renderActive = render

    override def render = {
      val ucs = RequestVars.UseCases.get
      val ucId = RequestVars.UseCaseId.get.value
      val isActive: (UseCaseSummary => Boolean) = (_.id == ucId)
      val currentUc = ucs.find(isActive) getOrElse shouldNeverHappen_!("SoleUseCaseId not found in UseCases")
      renderCurrent(currentUc) ++ dropdown(ucs filterNot isActive)
    }

    def renderCurrent(uc: UseCaseSummary): NodeSeq=
      <a href="#" data-toggle="dropdown" class="active-uc.dropdown-toggle">
        UC-<span class="num">{uc.number.value.toString}</span>: <span class="cur-uc-title">{uc.title}</span>
        <b class="caret"/>
      </a>

    def dropdown(ucs: List[UseCaseSummary]): NodeSeq =
      <ul class="dropdown-menu">{ucs foldMap dropdownLi}</ul>

    def dropdownLi(uc: UseCaseSummary): NodeSeq =
      <li><a href={AppSiteMap.UseCaseEditor.relativeUrl(uc.id)}>{uc.fullName}</a></li>
  }

  case class StaticText(text: String) extends NavbarElem {
    override val render = <a>{text}</a> % DudLinkAttr
    override def renderActive = render
  }
}
