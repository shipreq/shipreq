package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.client.project.app.Style.widgets.{splitScreen => *}

object SplitScreen {

  final case class Props(left: VdomNode, right: VdomNode) {
    @inline def render: VdomElement = Component(this)
  }

  val Component =
    ScalaFnComponent[Props] { p =>
      <.div(*.outer,
        <.section(*.left, p.left),
        <.section(*.right, p.right))
    }

}