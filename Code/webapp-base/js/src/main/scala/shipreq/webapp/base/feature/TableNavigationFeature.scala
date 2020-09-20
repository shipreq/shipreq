package shipreq.webapp.base.feature

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.lib.DomUtil

/** Allows users to navigate around a table using the keyboard.
  *
  * == Usage ==
  *
  *   1. Create a normal vdom table
  *   2. For th/td cells that can have focus and be reachable by keyboard
  *     i. Add `^.tabIndex := -1`
  *     i. Either add `TableNavigationFeature.{NoRowSpans,HasRowSpans}.onKeyDown`,
  *        or use `EditorNavParent`,
  *        or something like `^.onKeyDown ==> (e => TableNavigationFeature.Keys(e) | myOtherKeys(e))`.
  *        Composition of key handlers is a downstream responsibility.
  *   3. Add `&.focus(BaseStyles.focus.glowOutline)` to the cell's style.
  *   4. If the cell contains an editor, either
  *     i. add an `onClose` callback to focus the cell when the editor closes.
  *     i. use `EditorNavParent`
  *   5. If any cells are nested tables,
  *     i. The cell itself should still follow the rules above
  *     i. Add [[TableNavigationFeature.nestedTable]] to the nested table vdom.
  *   6. If a cell contains anything that should behave as a row, add [[TableNavigationFeature.newRow]] to it.
  *      An example of this is use case steps which are a bunch of divs in a single td cell.
  *
  * == Overriding navigation behaviour ==
  *
  * You can add special cases to the table that override the default behaviour.
  * This is currently being used in ReqDetail so that the title cell (which is outside of the table) can be navigated to
  * and from the table.
  *
  * 1. In the `componentDidMount` callback, call `TableNavigationFeature.SpecialCases(table)(handler)`
  *
  * That's it. The provided handler will take precedence over the default rules.
  */
object TableNavigationFeature {

  type Zipper = tablenav.TableCellZipper
  val  Zipper = tablenav.TableCellZipper

  type Movement = tablenav.Movement
  val  Movement = tablenav.Movement

  type Axis = tablenav.Axis
  val  Axis = tablenav.Axis

  type TableStyle = tablenav.TableStyle
  val  TableStyle = tablenav.TableStyle

  val SpecialCases = tablenav.SpecialCases

  def apply(ts: TableStyle): Bundle =
    if (ts.hasRowSpans)
      HasRowSpans
    else
      NoRowSpans

  /** Use this if the target table doesn't use rowSpans -- it's much faster */
  val NoRowSpans = new Bundle(TableStyle(hasRowSpans = false))

  /** Use this if the target table uses rowSpans */
  val HasRowSpans = new Bundle(TableStyle(hasRowSpans = true))

  /** Annotate nested tables with this to have them count as sub-cell content */
  val nestedTable: TagMod =
    VdomAttr(tablenav.Attrs.NestedTable) := 1

  val newRow: TagMod =
    VdomAttr(tablenav.Attrs.NewRow) := 1

  /** Mark a focusable element so that it is ignored by KB navigation. */
  val ignore: TagMod =
    ^.tabIndex := DomUtil.tabIndexIgnoreInt

  /** Mark an element so that itself and its children will be ignored by KB navigation. */
  val ignoreFamily: TagMod =
    VdomAttr(tablenav.Attrs.IgnoreFamily) := 1

  // -------------------------------------------------------------------------------------------------------------------

  final class Bundle private[TableNavigationFeature] (val tableStyle: TableStyle) {

    val Keys = new tablenav.TableNavKeys()(tableStyle)

    val onKeyDown: TagMod =
      ^.onKeyDown ==> Keys.handler
  }
}
