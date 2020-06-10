package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react.vdom.html_<^._

object Segment {

  val tag = <.div(^.cls := "ui segment")

  def fullHeight = tag(^.height := "100%")

}
