package shipreq.webapp.base.feature.tablenav

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.ReactExt_DomNode
import japgolly.univeq._
import org.scalajs.dom.html
import scalaz.{-\/, \/-}
import shipreq.base.util.Identity
import shipreq.webapp.base.lib.DomUtil._

object TableCellZipper {

  def option(o: Option[html.Element]): F[TableCellZipper] =
    o match {
      case Some(e) => \/-(apply(e))
      case None    => -\/("Nothing focusable")
    }

  def within(e: html.Element): F[TableCellZipper] =
    if (isFocusable(e))
      \/-(apply(e))
    else
      option(focusableChildren(e).nextOption())

  val allowMove: html.Element => Boolean = {
    // Allow checkboxes
    case i: html.Input => i.`type` ==* "checkbox"

    // These are not focusable by default, so if they are, I've specifically enabled them for this purpose
    case _: html.Div
         | _: html.Span
         | _: html.TableCell => true

    // Ignore non-whitelisted
    case _ => false
  }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final case class TableCellZipper(focus: html.Element) {
  import Logic._
  import TableCellZipper.allowMove

  private lazy val rootAndPos =
    findRootAndPos(parentsAndIndices(focus), false, focus)

  def root: F[html.Table] =
    rootAndPos.map(_._1)

  def focusPos: F[TablePos] =
    rootAndPos.map(_._2)

  def move(a: Axis, om: Option[Movement]): F[TableCellZipper] =
    om match {
      case None    => \/-(this)
      case Some(m) => move(a, m)
    }

  def move(a: Axis, m: Movement): F[TableCellZipper] =
      a match {
        case Axis.LeftRight =>
          for {
            pos        ← focusPos
            tr         ← rowAtPos(pos)
            rowResults = rowContentsIterator(tr).map(_._1).filter(allowMove).toVector
            i          ← findFocusIndex(rowResults)
            // TODO i should be lazy, movement might ignore it
          } yield TableCellZipper(m(rowResults, i))

        case Axis.UpDown =>
          for {
            pos      ← focusPos
            table    ← root
            tbody    ← table.child(pos.body)
            tr       ← tbody.child(pos.row)
            rows     = rowIterator(table).toVector
            rowIndex ← indexWhereF(rows)(_ eq tr, "Focus row not found")
          } yield {

            // This ignores moving up/down within the same cell
            // …which is fine for now because there's no logic to create more than one sub-row in a cell
            val newRow = m(rows, rowIndex)

            newRow.child(pos.cell) match {
              case \/-(cell) if isFocusable(cell) =>
                TableCellZipper(cell)

              case _ =>
                // Note: If I ever add logic to start making use of the Y in PosXY this logic will need some
                // adjustment. It doesn't consider height wrapping. If the user presses down in the last row and the
                // top row cells have sub-items with PosXY.y > 0 the distance fn will rank the bottom ones is being
                // closer to the currentFocus which doesn't make sense; in such a case the user would expect pressing
                // down from the very bottom will go the very top, not the bottom of the top-most row.
                val distRectFn = distanceRect(focus.getBoundingClientRect())
                val closest = rowContentsIterator(newRow)
                  .map(_._1)
                  .filter(allowMove)
                  .minBy(e => distRectFn(e.getBoundingClientRect()))
                TableCellZipper(closest)
            }
          }
      }

  /** Move horizontally within the same cell, if there is somewhere to move to.
    *
    * This is usually tab/shift-tab in table cells so users can jump in/out of text editors.
    * The reason that up/down/left/right doesn't automatically enter text editors is that those keys are used to
    * navigate the text itself inside the editor.
    * */
  def subMove(leftRight: Movement): F[Option[TableCellZipper]] =
    for {
      pos         ← focusPos
      tr          ← rowAtPos(pos)
      cellResults = rowContentsIterator(tr).filter(_._2._1 ==* pos.cell).map(_._1).toVector
      i           ← findFocusIndex(cellResults)
      // TODO i should be lazy, movement might ignore it & cellResults might preclude it
    } yield
      Option.when(cellResults.length > 1)(
        TableCellZipper(leftRight(cellResults, i)))

  def goto(pos: TablePos): F[TableCellZipper] =
    for {
      td     <- cellAtPos(pos)
      result <- pos.sub match {
        case None    => \/-(TableCellZipper(td))
        case Some(s) =>
          cellContentsIterator(td).find(_._2 ==* s) match {
            case Some((e, _)) => \/-(TableCellZipper(e))
            case None         => -\/(s"Nothing at $s")
          }
      }
    } yield result

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private def rowAtPos(pos: TablePos): F[html.TableRow] =
    for {
      table <- root
      tbody <- table.child(pos.body)
      tr    <- tbody.child(pos.row)
    } yield tr.domCast[html.TableRow]

  /** Ignores sub-pos */
  private def cellAtPos(pos: TablePos): F[html.TableCell] =
    for {
      tr <- rowAtPos(pos)
      td <- tr.child(pos.cell)
    } yield td.domCast[html.TableCell]

  private def findFocusIndex(as: IndexedSeq[html.Element]): F[Int] =
    findFocusIndexA(as)(Identity.apply)

  private def findFocusIndexA[A](as: IndexedSeq[A])(element: A => html.Element): F[Int] =
    indexWhereF(as)(element(_) eq focus, s"Focus not found: ${focus.outerHTML.take(255)}")
}
