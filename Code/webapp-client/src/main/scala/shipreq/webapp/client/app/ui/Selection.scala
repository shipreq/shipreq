package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.UnivEq
import shipreq.webapp.client.app.ui.Selection.Visible
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.util.{Off, On}

/**
 * Data selected by the user.
 */
sealed class Selection[A] private[Selection](val selected: Set[A]) {
  override def toString = s"Selection($selected)"

  def oneGet(a: A): On =
    On <~ selected.contains(a)

  def oneSet(a: A, newState: On): Selection[A] =
    newState match {
      case On  => Selection(selected + a)
      case Off => Selection(selected - a)
    }

  def oneCheckbox(a: A, set: Selection[A] => Callback): ReactTag = {
    val currentState = oneGet(a)
    UI.checkbox(currentState)(
      ^.onChange --> set(oneSet(a, !currentState)))
  }

  def oneToggle(a: A): Selection[A] =
    oneSet(a, !oneGet(a))

  def visible(visible: Set[A]): Visible[A] =
    new Visible(selected, visible)
}

object Selection {

  def apply[A](selected: Set[A]): Selection[A] =
    new Selection(selected)

  def empty[A: UnivEq]: Selection[A] =
    Selection(UnivEq.emptySet)

  final class Visible[A] private[Selection] (_selected: Set[A], val visible: Set[A]) extends Selection(_selected) {
    override def toString = s"Selection.Visible(\n  selected: $selected,\n  visible: $visible)"

    val (visibleSelection, hiddenSelection) =
      selected partition visible.contains

    val totalGet: Option[On] =
      if (visible.isEmpty)
        None
      else if (visibleSelection.isEmpty)
        Some(Off)
      else if (visibleSelection.size == visible.size)
        Some(On)
      else
        None

    def totalSet(newState: On): Selection[A] =
      newState match {
        case On  => Selection(hiddenSelection | visible)
        case Off => Selection(hiddenSelection)
      }

    def totalCheckbox(set: Selection[A] => Callback): ReactElement =
      Checkbox3.Component(Checkbox3.Props(totalGet, set compose totalSet))

    def totalToggle: Selection[A] =
      totalSet(Checkbox3 nextState totalGet)
  }

  implicit def reuseSel[A]: Reusability[Selection[A]] = Reusability.byRef || Reusability.by(_.selected)
  implicit def reuseVis[A]: Reusability[Visible[A]]   = Reusability.byRef || Reusability.by(v => (v.selected, v.visible))
}