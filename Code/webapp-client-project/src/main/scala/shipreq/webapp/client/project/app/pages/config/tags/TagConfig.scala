package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq
import shipreq.webapp.base.data._
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.client.project.widgets.{DropdownButton, ProjectWidgets, SplitScreenCrud}

object TagConfig {

  type NewState = NewTagType

  type EditorState = Unit

  val splitScreenCrud = new SplitScreenCrud[NewState, TagId, EditorState](
    initEditor = _ => Callback.empty, //: Id => CallbackTo[EditorState],
    rightEmpty = SplitScreenCrud.emptyEditorMessage("tag"),
  )

  val dropdownButton = new DropdownButton.Types[NewTagType]

  final case class Props(tags: Tags,
                         projectWidgets: ProjectWidgets.NoCtx,
                         state: StateSnapshot[State]) {
    @inline def render: VdomElement = Component(this)
  }

  sealed abstract class NewTagType(final val label: String) {
    final val item: dropdownButton.Item =
      DropdownButton.Item(label, this, label)
  }

  object NewTagType {
    case object Tag      extends NewTagType("Tag")
    case object TagGroup extends NewTagType("Tag group")

    implicit def univEq: UnivEq[NewTagType] = UnivEq.derive

    val values: NonEmptyVector[NewTagType] =
      AdtMacros.adtValuesManually[NewTagType](Tag, TagGroup)

    val items: NonEmptyVector[dropdownButton.Item] =
      values.map(_.item)
  }

  type State = splitScreenCrud.State

  def initState: State =
    splitScreenCrud.initState(NewTagType.Tag)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class Backend($: BackendScope[Props, Unit]) {
    import SplitScreenCrud.{NewArgs, ListArgs, EditorArgs}

    private def newButtonProps(args: splitScreenCrud.NewArgs): dropdownButton.DBProps =
      args match {

        case NewArgs.Disabled(sel) =>
          DropdownButton.Props.forNew[NewTagType](
            items    = NewTagType.items,
            selected = Some(sel),
            update   = None,
          )

        case NewArgs.Enabled(ss) =>
          DropdownButton.Props.forNew[NewTagType](
            items    = NewTagType.items,
            selected = Some(ss.value),
            update   = Some(Reusable.never(dropdownButton.Update( // TODO Reusable.never
              click  = _ => Callback.TODO,
              select = ss.setState,
            ))),
          )

      }

    private def renderLeft(p: Props, args: splitScreenCrud.ListArgs): VdomNode =
      NonEmptySet.option(p.tags.topLevelIds) match {
        case Some(ids) =>
          TagTreeView.Props(
            topLevelIds    = ids,
            tags           = p.tags,
            filterDead     = p.state.value.filterDead,
            selected       = args.selection,
            select         = args.enabledSelect,
            projectWidgets = p.projectWidgets,
          ).render

        case None =>
          NoTags.render
      }

    def render(p: Props): VdomNode = {
      val s = p.state.value

      splitScreenCrud(
        newButton = newButtonProps(_).render,
        list      = renderLeft(p, _),
        editor    = args => <.button("todo", ^.onClick --> args.close), // EditorArgs[Id, EditorState] => VdomNode,
        state     = p.state,
      )
    }
  }

  val Component = ScalaComponent.builder[Props]("TagConfig")
    .renderBackend[Backend]
    .build
}