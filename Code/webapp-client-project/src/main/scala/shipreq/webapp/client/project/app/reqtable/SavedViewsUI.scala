package shipreq.webapp.client.project.app.reqtable

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.ui.semantic.{Colour, Dropdown, Icon, Menu, SemExtAny}
import shipreq.webapp.base.ui.semantic.Dropdown.JsOptionsOps
import shipreq.webapp.client.project.app.Style.reqtable.{savedViews => *}

object SavedViewsUI {

  final case class Props(menu: SavedViewLogic.Menu) {
    @inline def render: VdomElement = Component(this)
  }

  //implicit val reusabilityProps: Reusability[Props] =
  //  Reusability.caseClass

  private val menuStyle =
    Menu.Style(
      Menu.Attr.Secondary + Menu.Attr.Pointing,
      tagMod = *.menu)

  private val dropdownOptions =
    Dropdown.JsOptions.default
      .onHover
      .withDelay(show = 300)

  private val defaultIcon =
    Icon.Star.withColour(Colour.Yellow).tag

  final class Backend($: BackendScope[Props, Unit]) {

    private def interpretMenu(m: SavedViewLogic.Menu): Menu.Props =
      Menu.Props(
        menuStyle,
        m.items.whole.map(i => interpretMenuItem(i, m.isActive(i))),
        dropdownOptions = dropdownOptions)

    private def interpretMenuItem(i: SavedViewLogic.Menu.Item, active: Boolean): Menu.Item = {
      val label: TagMod =
        if (i.default)
          TagMod(defaultIcon, i.name.value)
        else
          i.name.value

      val actions =
        i.actions.whole.map(interpretMenuAction)

      Menu.DropdownType.OnHover(label, actions).toItem(tagMod = *.activeItem.when(active))
    }

    private def interpretMenuAction(a: SavedViewLogic.Menu.Action): Dropdown.Item = {
      import SavedViewLogic.Menu.Action

      val n = SavedView.Name("XXX")

      val (icon, label, cmd) = a match {
        case Action.SaveAsNew  (c)    => (Icon.Plus , "Save as new...", c(n))
        case Action.Replace    (c, n) => (Icon.Save , "Replace " + n, c)
        case Action.MakeDefault(c)    => (Icon.Star , "Set as default", c)
        case Action.Delete     (c)    => (Icon.Trash, "Delete...", c)
        case Action.Rename     (c)    => (Icon.Write, "Rename...", c(n))
      }

      Dropdown.Item.Div(TagMod(icon.tag, label, ^.onClick --> Callback.alert(cmd.toString)))
    }

    def render(p: Props): VdomElement = {
      interpretMenu(p.menu).render
    }
  }

  val Component = ScalaComponent.builder[Props]("SavedViewsUI")
    .renderBackend[Backend]
    //.configure(Reusability.shouldComponentUpdate)
    .build
}
