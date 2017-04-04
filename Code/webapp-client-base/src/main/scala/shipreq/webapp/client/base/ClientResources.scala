package shipreq.webapp.client.base

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.AssetManifest

object ClientResources {

  val sortAscImg = <.img(^.src := AssetManifest.sortAscSvg, ^.alt := "Asc")

  val sortDescImg = <.img(^.src := AssetManifest.sortAscSvg, ^.alt := "Desc", ^.transform := "scaleY(-1)")

  val sortBlankImg = <.img(^.src := AssetManifest.sortBlankSvg, ^.alt := "Blanks")

  val spinnerImg = <.img(^.src := AssetManifest.loadingSpinSvg, ^.cls := "spinner")

}
