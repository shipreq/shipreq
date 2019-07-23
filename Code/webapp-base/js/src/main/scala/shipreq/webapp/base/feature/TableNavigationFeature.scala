package shipreq.webapp.base.feature

import japgolly.scalajs.react.vdom.html_<^._

/** Allows users to navigate around a table using the keyboard.
  *
  * == Usage ==
  *
  *   1. Create a normal vdom table
  *   2. For th/td cells that can have focus and be reachable by keyboard
  *     i. Add `^.tabIndex := -1`
  *     i. Either add `TableNavigationFeature.onKeyDown`,
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
  */
object TableNavigationFeature {

  val Keys = tablenav.TableNavKeys

  type Zipper = tablenav.TableCellZipper
  val  Zipper = tablenav.TableCellZipper

  type Movement = tablenav.Movement
  val  Movement = tablenav.Movement

  type Axis = tablenav.Axis
  val  Axis = tablenav.Axis

  /** Annotate nested tables with this to have them count as sub-cell content */
  val nestedTable: TagMod =
    VdomAttr(tablenav.Attrs.NestedTable) := 1

  val newRow: TagMod =
    VdomAttr(tablenav.Attrs.NewRow) := 1

  val onKeyDown: TagMod =
    ^.onKeyDown ==> Keys.handler
}
