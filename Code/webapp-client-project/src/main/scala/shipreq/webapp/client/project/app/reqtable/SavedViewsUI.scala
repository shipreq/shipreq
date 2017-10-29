package shipreq.webapp.client.project.app.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.reqtable.SavedView
import shipreq.webapp.base.protocol.SavedViewCmd
import shipreq.webapp.base.ui.semantic.Dropdown.JsOptionsOps
import shipreq.webapp.base.ui.semantic.{Colour, Dropdown, Icon, Menu => SemUiMenu, SemExtAny}
import shipreq.webapp.client.project.app.Style.reqtable.{savedViews => *}
import SavedViewLogic._

object SavedViewsUI {

  final case class Props(menu     : Menu,
                         runAction: Action ~=> Callback,
                         runCmd   : SavedViewCmd ~=> Callback) {
    @inline def render: VdomElement = Component(this)
  }

  //implicit val reusabilityProps: Reusability[Props] =
  //  Reusability.caseClass

  private val menuStyle =
    SemUiMenu.Style(
      SemUiMenu.Attr.Secondary + SemUiMenu.Attr.Pointing,
      tagMod = *.menu)

  private val dropdownOptions =
    Dropdown.JsOptions.default
      .onHover
      .withDelay(show = 500)

  private val defaultIcon =
    Icon.Star.withColour(Colour.Yellow).tag

  final class Backend($: BackendScope[Props, Unit]) {

    private def interpretMenu(interpretMenuItem: (MenuItem, Boolean) => SemUiMenu.Item)
                             (m: Menu): SemUiMenu.Props =
      SemUiMenu.Props(
        menuStyle,
        m.items.whole.map(i => interpretMenuItem(i, m.isActive(i))),
        dropdownOptions = dropdownOptions)

    private def interpretMenuItem(runAction          : Action => Callback,
                                  interpretMenuAction: MenuAction => Dropdown.Item)
                                 (mi                 : MenuItem,
                                  active             : Boolean): SemUiMenu.Item = {
      val label: TagMod =
        if (mi.default)
          TagMod(defaultIcon, mi.name.value)
        else
          mi.name.value

      val actions =
        mi.actions.whole.map(interpretMenuAction)

      SemUiMenu.DropdownType.OnHover(label, actions)
        .toItem(tagMod = *.activeItem.when(active))
        .withOnClick($.getDOMNode, mi.optionId.map(id => runAction(Action.Select(id))).getOrEmpty)
    }

    private def interpretMenuAction(runCmd: SavedViewCmd ~=> Callback): MenuAction => Dropdown.Item = {

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

      {
        case MenuAction.SaveAsNew(cmdFn) =>
          val cb = promptThenRun(
            CallbackTo.prompt("Enter a name for this view"),
            cmdFn(_).map(Some(_)).toEither)
          item(Icon.Plus, "Save as new...", cb)

        case MenuAction.Replace(name, cmd) =>
          item(Icon.Save, s"Replace ${name.value}", runCmd(cmd))

        case MenuAction.MakeDefault(cmd) =>
          item(Icon.Star, "Set as default", runCmd(cmd))

        case MenuAction.Delete(name, cmd) =>
          val cb = for {
            _ <- CallbackTo.confirm(s"Really delete the '${name.value}' view?").requireCBO
            _ <- runCmd(cmd).toCBO
          } yield ()
          item(Icon.Trash, "Delete...", cb)

        case MenuAction.Rename(name, cmdFn) =>
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
