package shipreq.webapp.base.feature

import japgolly.scalajs.react.vdom.html_<^._

/** Allows users to navigate around a table using the keyboard.
  *
  * == Usage ==
  *
  *   1. Create a normal vdom table
  *   2. For th/td cells that can have focus and be reachable by keyboard
  *     i. Add {{{^.tabIndex := -1}}}
  *     i. Add either {{{^.onKeyDown ==> TableNavigationFeature.Keys.handler}}},
  *        or something like {{{^.onKeyDown ==> (e => TableNavigationFeature.Keys(e) | EditorFeature.Keys(editor)(e))}}}.
  *        Composition of key handlers is a downstream responsibility.
  *   3. If any cells are nested tables,
  *     i. The cell itself should still follow the rules above
  *     i. Add [[TableNavigationFeature.nestedTable]] to the nested table vdom.
  *   4. If a cell contains anything that should behave as a row, add [[TableNavigationFeature.newRow]] to it.
  *      An example of this is use case steps which are a bunch of divs in a single td cell.
  */
object TableNavigationFeature {

  /** Annotate nested tables with this to have them count as sub-cell content */
  val nestedTable: TagMod =
    VdomAttr(tablenav.Attrs.NestedTable) := 1

  val newRow: TagMod =
    VdomAttr(tablenav.Attrs.NewRow) := 1

  val Keys = tablenav.TableNavKeys

  type Zipper = tablenav.TableCellZipper
  val  Zipper = tablenav.TableCellZipper

  type Movement = tablenav.Movement
  val  Movement = tablenav.Movement

  type Axis = tablenav.Axis
  val  Axis = tablenav.Axis
}
