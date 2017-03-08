package shipreq.webapp.client.project.app

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.URLs._

object Assets {
  val sortSvgAsc   = <.img(^.src := SvgSortAsc, ^.alt := "Asc")
  val sortSvgDesc  = <.img(^.src := SvgSortAsc, ^.alt := "Desc", ^.transform := "scaleY(-1)")
  val sortSvgBlank = <.img(^.src := SvgSortBlank, ^.alt := "Blanks")

  val spinner = <.img(^.src := SvgSpinner, ^.cls := "spinner")
}
