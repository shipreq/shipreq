package shipreq.webapp.client.project.app.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.client.project.app.Style.reqtable.{page => *}
import shipreq.webapp.client.base.ui.semantic.{Menu, SemExtAny}
import shipreq.webapp.client.project.widgets.FilterDeadButton

/** The row at the body of the page that has
  * saved views on the left, and the FilterDead button on the right.
  */
object ViewsMenu {

  type Props = Option[FilterDeadButton.Props]

  private val style =
    Menu.Style(Menu.Attr.Secondary + Menu.Attr.Pointing)

  private val leftItems: Menu.Items =
    Menu.Item.Link(
      <.a(
        "Unsaved view",
        ^.color := "#888",
        ^.onClick --> Callback.alert("This feature isn't implemented yet."))) :: Nil

  private def render(p: Props): VdomElement = {
    val filterDeadButton = Menu.Item.Div(
      TagMod(
        *.filterDeadButtonContainer,
        p match {
          case Some(pp) => FilterDeadButton.Component(pp)
          case None     => FilterDeadButton.ForceHideDead
        }))

    Menu.Props(
      style,
      leftItems,
      filterDeadButton :: Nil)
      .render
  }

  val Component = ScalaComponent.builder[Props]("ViewsMenu")
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}