package shipreq.webapp.client.feature

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import shipreq.base.util.UnivEq
import shipreq.webapp.client.data.{Off, On}
import shipreq.webapp.client.widgets.{Checkbox3, Widgets}
import Selection._

/**
 * Data selected by the user.
 */
final class Selection[A] private[Selection](val selected: Set[A]) extends Selection.Base[A] {
  override def toString = s"Selection($selected)"

  def updateBy(f: UpdateFn[A]): WithUpdateFn[A] =
    new WithUpdateFn(selected, f)

  def updateByNoReuse(f: Selection[A] => Callback): WithUpdateFn[A] =
    updateBy(ReusableFn(f))
}

object Selection {
  def apply[A](selected: Set[A]): Selection[A] =
    new Selection(selected)

  def empty[A: UnivEq]: Selection[A] =
    Selection(UnivEq.emptySet)

  type UpdateFn[A] = Selection[A] ~=> Callback

  // ===================================================================================================================
  // Traits
  // ===================================================================================================================

  sealed trait Base[A] {
    val selected: Set[A]

    def clearAll(as: TraversableOnce[A]): Selection[A] =
      Selection(selected -- as)
  }

  sealed trait HasUpdateFn[A] extends Base[A] {
    val updateFn: UpdateFn[A]
  }

  sealed trait HasLegalSubset[A] extends Base[A] {
    val legal: Set[A]

    val (legalSelection, hiddenSelection) =
      selected partition legal.contains
  }

  // These are specialised and so don't extend Base

  sealed trait Focus[A, Get] {
    val get: Get
    def set(n: On): Selection[A]
    def toggle: Selection[A]
  }

  sealed trait UI[A, Get, Checkbox] extends Focus[A, Get] {
    val updateFn: UpdateFn[A]
    def toggleFn: Callback = updateFn(toggle)
    def checkbox: Checkbox
    def checkboxAndOnClick: TagMod

    final def onClick: TagMod =
      TagMod (^.onClick --> toggleFn, ^.cursor.pointer)
  }

  // ===================================================================================================================
  // Classes
  // ===================================================================================================================

  final class WithUpdateFn[A] private[Selection](val selected: Set[A], val updateFn: UpdateFn[A])
      extends HasUpdateFn[A] {

    override def toString = s"Selection($selected)"

    def apply(a: A) =
      new OneUI(a, selected, updateFn)

    def legal(legal: Set[A]) =
      new LegalWithUpdateFn(selected, legal, updateFn)
  }

  // -------------------------------------------------------------------------------------------------------------------

  final class LegalWithUpdateFn[A] private[Selection](val selected: Set[A], val legal: Set[A], val updateFn: UpdateFn[A])
      extends HasUpdateFn[A] with HasLegalSubset[A] {

    override def toString = s"Selection.Legal(\n  selected: $selected,\n  legal: $legal)"

    def apply(a: A) =
      new OneUI(a, selected, updateFn)

    val total = new TotalUI(legal, legalSelection, hiddenSelection, updateFn)
  }

  // -------------------------------------------------------------------------------------------------------------------

  final class OneUI[A](a: A, selected: Set[A], override val updateFn: UpdateFn[A]) extends UI[A, On, ReactTag] {
    override val get =
      On <~ selected.contains(a)

    override def set(newState: On) =
      newState match {
        case On  => Selection(selected + a)
        case Off => Selection(selected - a)
      }

    override def toggle =
      set(!get)

    override def checkbox =
      Widgets.checkbox(get)(^.onChange --> toggleFn)

    override def checkboxAndOnClick: TagMod =
      TagMod(checkbox, onClick)
  }

  // -------------------------------------------------------------------------------------------------------------------

  final class TotalUI[A](legal: Set[A], legalSelection: Set[A], hiddenSelection: Set[A],
                         override val updateFn: UpdateFn[A]) extends UI[A, Option[On], ReactElement] {
    override val get =
      if (legal.isEmpty)
        None
      else if (legalSelection.isEmpty)
        Some(Off)
      else if (legalSelection.size == legal.size)
        Some(On)
      else
        None

    override def set(newState: On): Selection[A] =
      newState match {
        case On  => Selection(hiddenSelection | legal)
        case Off => Selection(hiddenSelection)
      }

    override def toggle: Selection[A] =
      set(Checkbox3 nextState get)

    override def checkbox: ReactElement =
      Checkbox3.Component(Checkbox3.Props(get, updateFn compose set))

    override def checkboxAndOnClick: TagMod =
      TagMod(checkbox, onClick)
  }

  // ===================================================================================================================

  implicit def reuseSel[A]: Reusability[Selection[A]]         = Reusability.byRef || Reusability.by(_.selected)
  implicit def reuseVis[A]: Reusability[LegalWithUpdateFn[A]] = Reusability.byRef || Reusability.by(v => (v.selected, v.legal, v.updateFn))
}
