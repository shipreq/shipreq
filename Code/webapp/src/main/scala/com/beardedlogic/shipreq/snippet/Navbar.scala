package shipreq.webapp.snippet

import scala.xml._
import shipreq.webapp.app.RequestVars
import shipreq.webapp.feature.NavbarElem

/**
 * Renders a Navbar according to the contents of `RequestVars.Navbar`.
 */
object Navbar {
  def render: NodeSeq = {
    val navbar = RequestVars.Navbar.get
    val h = renderActive(navbar.elemsReversed.head)
    val t = navbar.elemsReversed.tail.map(renderInactive)
    (h /: t)((acc, li) => li ++ acc)
  }

  def renderActive(e: NavbarElem): NodeSeq   = e customiseLi <li class="active">{e.renderActive}</li>
  def renderInactive(e: NavbarElem): NodeSeq = e customiseLi <li>{e.render}</li>
}