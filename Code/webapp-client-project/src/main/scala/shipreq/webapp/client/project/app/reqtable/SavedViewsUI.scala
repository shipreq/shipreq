package shipreq.webapp.client.project.app.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.reqtable.SavedView
import shipreq.webapp.base.protocol.SavedViewCmd
import shipreq.webapp.base.ui.semantic.Dropdown.JsOptionsOps
import shipreq.webapp.base.ui.semantic.{Colour, Dropdown, Icon, Menu, SemExtAny}
import shipreq.webapp.client.project.app.Style.reqtable.{savedViews => *}

object SavedViewsUI {

  final case class Props(menu     : SavedViewLogic.Menu,
                         runAction: SavedViewLogic.Action ~=> Callback,
                         runCmd   : SavedViewCmd ~=> Callback) {
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
      .withDelay(show = 500)

  private val defaultIcon =
    Icon.Star.withColour(Colour.Yellow).tag

  final class Backend($: BackendScope[Props, Unit]) {

    private def interpretMenu(interpretMenuItem: (SavedViewLogic.Menu.Item, Boolean) => Menu.Item)
                             (m: SavedViewLogic.Menu): Menu.Props =
      Menu.Props(
        menuStyle,
        m.items.whole.map(i => interpretMenuItem(i, m.isActive(i))),
        dropdownOptions = dropdownOptions)

    private def interpretMenuItem(runAction          : SavedViewLogic.Action => Callback,
                                  interpretMenuAction: SavedViewLogic.Menu.Action => Dropdown.Item)
                                 (i                  : SavedViewLogic.Menu.Item,
                                  active             : Boolean): Menu.Item = {
      val label: TagMod =
        if (i.default)
          TagMod(defaultIcon, i.name.value)
        else
          i.name.value

      val actions =
        i.actions.whole.map(interpretMenuAction)

      Menu.DropdownType.OnHover(label, actions)
        .toItem(tagMod = *.activeItem.when(active))
        .withOnClick($.getDOMNode, i.optionId.map(id => runAction(SavedViewLogic.Action.Select(id))).getOrEmpty)
    }

    private def interpretMenuAction(runCmd: SavedViewCmd ~=> Callback)
                                   (a: SavedViewLogic.Menu.Action): Dropdown.Item = {
      import SavedViewLogic.Menu.Action

      def item(icon: Icon, label: String, onClick: Callback) =
        Dropdown.Item.Div(TagMod(icon.tag, label, ^.onClick --> onClick))

      def promptThenRun(prompt: CallbackTo[Option[String]],
                        validate: String => Either[String, Option[SavedViewCmd]]): CallbackOption[Unit] =
        for {
          cmd <- CallbackTo.retryUntilRight(
                   prompt.map {
                     case Some(s) => validate(s)
                     case None    => Right(None)
                   }
                 )(Callback.alert).asCBO
          _ <- runCmd(cmd).toCBO
        } yield ()

      a match {

        case Action.SaveAsNew(cmdFn) =>
          val cb = promptThenRun(
            CallbackTo.prompt("Enter a name for this view"),
            cmdFn(_).map(Some(_)).toEither)
          item(Icon.Plus, "Save as new...", cb)

        case Action.Replace(name, cmd) =>
          item(Icon.Save, s"Replace ${name.value}", runCmd(cmd))

        case Action.MakeDefault(cmd) =>
          item(Icon.Star, "Set as default", runCmd(cmd))

        case Action.Delete(name, cmd) =>
          val cb = for {
            _ <- CallbackTo.confirm(s"Really delete the '${name.value}' view?").requireCBO
            _ <- runCmd(cmd).toCBO
          } yield ()
          item(Icon.Trash, "Delete...", cb)

        case Action.Rename(name, cmdFn) =>
          val cb = promptThenRun(
            CallbackTo.prompt(s"Enter a new name for ${name.value}", name.value),
            cmdFn(_).toDisjOption.toEither)
          item(Icon.Write, "Rename...", cb)
      }
    }

    def render(p: Props): VdomElement = {
      interpretMenu(
        interpretMenuItem(p.runAction,
          interpretMenuAction(p.runCmd)))(p.menu).render
    }
  }

  val Component = ScalaComponent.builder[Props]("SavedViewsUI")
    .renderBackend[Backend]
    //.configure(Reusability.shouldComponentUpdate) TODO
    .build
}
