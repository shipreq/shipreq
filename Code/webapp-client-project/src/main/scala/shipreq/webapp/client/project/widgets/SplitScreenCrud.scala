package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.MonocleReact._
import monocle.macros.Lenses
import scala.reflect.ClassTag
import shipreq.webapp.base.data._
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/, \/-}
import shipreq.webapp.client.project.app.Style.widgets.{splitScreenCrud => *}


/** Takes care of a common pattern in config screens where...
  *
  * - the screen is split left and right
  * - on the left is...
  *    - a new button
  *    - a filter-dead button
  *    - a list of all items (or a msg if empty)
  * - users can select an item on the left to open an editor on the right
  * - on the right is one of the following...
  *    - an editor
  *    - instructions tp the user
  *
  * - States:
  *
  *   - nothing selected
  *     - new enabled
  *     - nothing highlighted in list
  *     - empty on right
  *
  *   - item selected
  *     - new disabled
  *     - highlight in list
  *     - editor on right
  *       - form, delete, (cancel | close), update (enabled | disabled) buttons on edit live
  *       - restore, close buttons on edit dead
  *       - msg, cancel on new (impossible)
  *       - form, cancel, create (enabled | disabled) on new (possible)
  */
object SplitScreenCrud {

  sealed trait NewArgs[S]
  object NewArgs {
    final case class Disabled[S](value: S)                                      extends NewArgs[S]
    final case class Enabled [S](state: StateSnapshot[S], openEditor: Callback) extends NewArgs[S]
  }

  sealed trait ListArgs[Id] {
    def selection: Option[Id]
    def enabledSelect: Option[Id ~=> Callback]
    def closeEditor: Option[Reusable[Callback]]
  }

  object ListArgs {
    final case class Disabled[Id](selection: Option[Id],
                                  closeEditor: Option[Reusable[Callback]]) extends ListArgs[Id] {
      override def enabledSelect = None
    }

    final case class Enabled[Id](select: Id ~=> Callback) extends ListArgs[Id] {
      override def selection = None
      override def enabledSelect = Some(select)
      override def closeEditor = None
    }
  }

  final case class EditorArgs[+N, +Id, S](id   : N \/ Id,
                                          state: StateSnapshot[S],
                                          close: Callback)

  final case class Props[N, Id, E](filterDeadOverride: Option[FilterDead],
                                   newButton         : NewArgs[N] => VdomNode,
                                   list              : ListArgs[Id] => VdomNode,
                                   editor            : EditorArgs[N, Id, E] => VdomNode,
                                   initEditor        : (N \/ Id) => CallbackTo[E],
                                   state             : StateSnapshot[State[N, Id, E]])

  @Lenses
  final case class State[N, Id, E](newState  : N,
                                   filterDead: FilterDead,
                                   right     : State.Right[Id, E])

  object State {

    sealed trait Right[+Id, +EditorState] {
      final val editorOption: Option[EditorState] =
        this match {
          case Right.Empty        => None
          case Right.Create(s)    => Some(s)
          case Right.Update(_, s) => Some(s)
        }

      final val idOption: Option[Id] =
        this match {
          case Right.Empty
             | Right.Create(_)     => None
          case Right.Update(id, _) => Some(id)
        }
    }

    object Right {
      case object      Empty                              extends Right[Nothing, Nothing]
      final case class Create[+S]     (editor: S)         extends Right[Nothing, S]
      final case class Update[+Id, +S](id: Id, editor: S) extends Right[Id, S]
    }
  }

//    implicit def reusabilityNewArgs [S: Reusability]: Reusability[NewArgs[S]] = Reusability.derive
//    implicit def reusabilityProps[N, I, E]: Reusability[Props[N, I, E]] = Reusability.derive
//    implicit def reusabilityState[N, I, E]: Reusability[State[N, I, E]] = Reusability.derive

  def emptyEditorMessage(noun: String): VdomNode = {
//    This is the tag editor.
//    Create a new tag, or select an existing tag to edit it here.
    <.div(*.emptyRight,
      <.div(*.emptyRightHeader),
      <.div(*.emptyRightBody,
        <.div(s"This is the $noun editor."),
        <.div(s"Create a new $noun, or select an existing $noun to edit it here.")))
  }
}


// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final class SplitScreenCrud[
    NewState,
    Id         : ClassTag : Reusability,
    EditorState,
  ](
    rightEmpty: VdomNode,
  ) {

  import SplitScreenCrud.{State => S}

  type Props      = SplitScreenCrud.Props[NewState, Id, EditorState]
  type State      = SplitScreenCrud.State[NewState, Id, EditorState]
  type NewArgs    = SplitScreenCrud.NewArgs[NewState]
  type ListArgs   = SplitScreenCrud.ListArgs[Id]
  type EditorArgs = SplitScreenCrud.EditorArgs[NewState, Id, EditorState]

  @inline def apply(filterDeadOverride: Option[FilterDead],
                    newButton         : NewArgs    => VdomNode,
                    list              : ListArgs   => VdomNode,
                    editor            : EditorArgs => VdomNode,
                    initEditor        : (NewState \/ Id) => CallbackTo[EditorState],
                    state             : StateSnapshot[State]): VdomNode =
    Component(SplitScreenCrud.Props(filterDeadOverride, newButton, list, editor, initEditor, state))

  def initState(newState: NewState): State =
    S(newState, HideDead, S.Right.Empty)

  private val newStateLens = S.newState[NewState, Id, EditorState]

  private val renderRightEmpty: On => VdomNode =
    On.memo {
      case On  => <.div(rightEmpty, *.rightOn)
      case Off => <.div(rightEmpty, *.rightOff)
    }

  sealed class Backend($: BackendScope[Props, Unit]) {

    private val select: Id ~=> Callback =
      Reusable.byRef(
        id =>
          for {
            p           <- $.props
            editorState <- p.initEditor(\/-(id))
            right        = S.Right.Update(id, editorState)
            _           <- p.state.modState(_.copy(right = right))
          } yield ()
      )

    private val openNewEditor: NewState ~=> Callback =
      Reusable.byRef(
        ns =>
          for {
            p           <- $.props
            editorState <- p.initEditor(-\/(ns))
            right        = S.Right.Create(editorState)
            _           <- p.state.modState(_.copy(right = right))
          } yield ()
      )

    private def modEditorState(f: (State, EditorState) => State): StateSnapshot.SetFn[EditorState] =
      (os2, cb) =>
        for {
          p <- $.props
          _ <- p.state.modStateOption(s => os2.map(f(s, _)), cb)
        } yield ()

    private val setCreateState: Reusable[StateSnapshot.SetFn[EditorState]] =
      Reusable.byRef(modEditorState((s, es) => s.copy(right = S.Right.Create(es))))

    private def setUpdateState(id: Id): Reusable[StateSnapshot.SetFn[EditorState]] =
      Reusable.implicitly(id).withValue(
        modEditorState((s, es) => s.copy(right = S.Right.Update(id, es))))

    private val closeEditor: Reusable[Callback] =
      Reusable.callbackByRef(
        $.props.flatMap(_.state.modState(_.copy(right = S.Right.Empty))))

    def render(p: Props): VdomNode = {
      val s = p.state.value

      val newButtonEnabled: Enabled =
        s.right match {
          case S.Right.Empty           => Enabled
          case _: S.Right.Create[_]    => Enabled
          case _: S.Right.Update[_, _] => Disabled
        }

      val newArgs: NewArgs =
        newButtonEnabled match {
          case Enabled =>
            SplitScreenCrud.NewArgs.Enabled(
              state      = p.state.zoomStateL(newStateLens),
              openEditor = Callback.byName(openNewEditor(p.state.value.newState)))

          case Disabled =>
            SplitScreenCrud.NewArgs.Disabled(p.state.value.newState)
        }

      val newButton: VdomNode =
        p.newButton(newArgs)

      val filterDeadButton: VdomNode =
        p.filterDeadOverride match {
          case None     => FilterDeadButton.Component(p.state.zoomStateL(S.filterDead))
          case Some(fd) => FilterDeadButton.Component(StateSnapshot(fd)((_, _) => Callback.empty))
        }


      val listArgs: ListArgs =
        s.right match {
          case S.Right.Empty         => SplitScreenCrud.ListArgs.Enabled(select)
          case S.Right.Create(_)     => SplitScreenCrud.ListArgs.Disabled(None, None)
          case S.Right.Update(id, _) => SplitScreenCrud.ListArgs.Disabled(Some(id), Some(closeEditor))
        }

      val leftBody: VdomNode =
        p.list(listArgs)

      val editorArgs: Option[EditorArgs] =
        s.right match {
          case S.Right.Empty =>
            None

          case S.Right.Create(es) =>
            Some(SplitScreenCrud.EditorArgs(-\/(s.newState), StateSnapshot(es)(setCreateState), closeEditor))

          case S.Right.Update(id, es) =>
            Some(SplitScreenCrud.EditorArgs(\/-(id), StateSnapshot(es)(setUpdateState(id)), closeEditor))
        }

      val left: VdomNode =
        React.Fragment(
          <.div(*.topLeft,
            <.div(*.topLeftGrow, newButton),
            <.div(filterDeadButton)),
          leftBody)

      val right: VdomNode =
        editorArgs match {
          case None =>
            React.Fragment(
              renderRightEmpty(On),
              // TODO capture lastEditor - use with .rightOff
            )

          case Some(args) =>
            React.Fragment(
              renderRightEmpty(Off),
              <.div(*.rightOn, p.editor(args)))
        }

      SplitScreen.Props(left = left, right = right).render
    }
  }

  val Component = ScalaComponent.builder[Props]("SplitScreenCrud")
    .renderBackend[Backend]
    .build
}