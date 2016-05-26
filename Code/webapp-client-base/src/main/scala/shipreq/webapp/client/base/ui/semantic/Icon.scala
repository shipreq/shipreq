package shipreq.webapp.client.base.ui.semantic

import japgolly.scalajs.react.vdom.prefix_<^._

/** http://semantic-ui.com/elements/icon.html */
sealed abstract class Icon(c: ClassName) {
  final val tag = <.i(^.cls := ("icon " + c))
}

object Icon {
  case object Cubes extends Icon("cubes")
  case object Write extends Icon("write")
}
