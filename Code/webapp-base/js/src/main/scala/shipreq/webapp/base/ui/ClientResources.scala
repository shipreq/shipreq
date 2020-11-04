package shipreq.webapp.base.ui

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.config.AssetManifest

final class ClientResources(am: AssetManifest) {

  val sortAscImg = <.img(^.src := am.sortAscSvg, ^.alt := "Asc")

  val sortDescImg = <.img(^.src := am.sortAscSvg, ^.alt := "Desc", ^.transform := "scaleY(-1)")

  val sortBlankImg = <.img(^.src := am.sortBlankSvg, ^.alt := "Blanks")

  val spinnerImg = <.img(^.src := am.loadingSpinSvg, ^.cls := "spinner")

}
