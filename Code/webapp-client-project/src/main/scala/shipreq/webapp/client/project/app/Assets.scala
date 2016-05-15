package shipreq.webapp.client.project.app

import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.WebappConfig.assetPath

object Assets {
  val sortSvgAsc   = <.img(^.src := s"$assetPath/sort-asc.svg", ^.alt := "Asc")
  val sortSvgDesc  = <.img(^.src := s"$assetPath/sort-asc.svg", ^.alt := "Desc", ^.transform := "scaleY(-1)")
  val sortSvgBlank = <.img(^.src := s"$assetPath/sort-blank.svg", ^.alt := "Blanks")

  val spinner =
    <.img(
      ^.cls := "spinner",
      ^.src := s"$assetPath/loading-spin.svg")
}
