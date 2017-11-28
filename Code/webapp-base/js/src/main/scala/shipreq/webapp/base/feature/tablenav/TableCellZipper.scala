package shipreq.webapp.base.feature.tablenav

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.ReactExt_DomNode
import japgolly.univeq._
import org.scalajs.dom.html
import scalaz.{-\/, \/-}
import shipreq.base.util.Identity
import shipreq.base.util.ScalaExt._
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
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final case class TableCellZipper(focus: html.Element) {
  import Logic._

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
            rows     = movableRowIterator(table).toVector
            rowIndex ← indexWhereF(rows)(_ eq tr, "Focus row not found")
          } yield {

            // This ignores moving up/down within the same cell
            // …which is fine for now because there's no logic to create more than one sub-row in a cell
            val newRow = m(rows, rowIndex)

            def focusClosestInNewRow: TableCellZipper =
              focusClosest(focus, rowContentsIterator(newRow).map(_._1).filter(allowMove))
                .get // newRow is proven not to be empty via .filter in movableRowIterator

            newRow.child(pos.cell) match {
              case \/-(cell) =>
                pos.sub match {
                  case None =>
                    if (isFocusable(cell))
                      TableCellZipper(cell)
                    else
                      focusableChildren(cell).filter(allowMove).nextOption() match {
                        case Some(sub) => TableCellZipper(sub)
                        case None      => focusClosestInNewRow
                      }
                  case Some(xy) =>
                    cellContentsIterator(cell, false)
                      .filter(_._1 |> allowMove)
                      .find(_._2.exists(_ ==* xy)) match {
                      case Some((sub, _)) =>
                        TableCellZipper(sub)
                      case None =>
                        focusClosest(focus, cellContentsIterator(cell, true).map(_._1).filter(allowMove))
                          .getOrElse(focusClosestInNewRow)
                    }
                }

              case -\/(_) =>
                focusClosestInNewRow
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
          cellContentsIterator(td, false).find(_._2.exists(_ ==* s)) match {
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
