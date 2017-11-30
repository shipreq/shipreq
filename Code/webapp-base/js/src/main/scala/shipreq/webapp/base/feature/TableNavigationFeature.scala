package shipreq.webapp.base.feature

import japgolly.scalajs.react.vdom.html_<^._

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
