package com.beardedlogic.usecase.snippet

import scala.xml._
import com.beardedlogic.usecase.app.RequestVars
import com.beardedlogic.usecase.feature.NavbarElem

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