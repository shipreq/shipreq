package shipreq.webapp.base.feature.tablenav

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.ReactExt_DomNode
import japgolly.univeq._
import org.scalajs.dom.html
import scalaz.{-\/, \/-}
import scalaz.std.option.optionInstance
import scalaz.syntax.traverse._
import shipreq.base.util.{Deny, Identity}
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

        case Axis.UpDown =>
          for {
            pos      ← focusPos
            table    ← root
            tbody    ← table.child(pos.body)
            tr       ← tbody.child(pos.row)
            rows     ← needNev(movableRowIterator(table), "no rows")
                       // This ignores moving up/down within the same cell
                       // …which is fine for now because there's no logic to create more than one sub-row in a cell
            rowIdxF  = indexWhereF(rows.whole)(_ eq tr, "Focus row not found")
            newRow   ← m.moveNevF(rows, rowIdxF)
          } yield {

            def focusClosestInNewRow: TableCellZipper =
              focusClosest(focus, rowMovableElementsIterator(newRow)).get // newRow is proven not to be empty via .filter in movableRowIterator

            newRow.child(pos.cell) match {

              case \/-(cell) =>
                def subMovables = focusableChildren(cell).filter(allowMove)
                def whenNoSubs = if (isFocusable(cell)) TableCellZipper(cell) else focusClosestInNewRow

                pos.sub match {
                  case None =>
                    subMovables.nextOption() match {
                      case Some(sub) => TableCellZipper(sub)
                      case None      => whenNoSubs
                    }
                  case Some(xy) =>
                    subAt(cell, xy).filter(allowMove) match {
                      case Some(sub) => TableCellZipper(sub)
                      case None      => focusClosest(focus, subMovables).getOrElse(whenNoSubs)
                    }
                }

              case -\/(_) =>
                focusClosestInNewRow
            }
          }

        case Axis.LeftRight =>
          for {
            pos        ← focusPos
            tr         ← rowAtPos(pos)
            rowResults = rowContentsIterator(tr).filter(_._1 |> allowMove).toVector
            indexF     = findFocusIndexA(rowResults)(_._1)
            target     ← m.moveSelectiveF(rowResults, indexF)((a, i) => allowMove2(a, rowResults.get(i + 1)))
          } yield target.fold(this)(t => TableCellZipper(t._1))
      }

  /** Move horizontally within the same cell, if there is somewhere to move to.
    *
    * This is usually tab/shift-tab in table cells so users can jump in/out of text editors.
    * The reason that up/down/left/right doesn't automatically enter text editors is that those keys are used to
    * navigate the text itself inside the editor.
    * */
  def subMove(leftRight: Movement): F[Option[TableCellZipper]] = {

    // Removes the outer cell from consideration if there are sub-cells allowed by allowMove (i.e. arrow-keys)
    def excludeOuter(cell: NonEmptyVector[RowContent]): (NonEmptyVector[RowContent], F[Int]) = {
      def subMoveable = cell.iterator.drop(1).filter(_._1 |> allowMove).nextOption()
      val cell2 =
        cell.tailNonEmpty match {
          case Some(tail) if allowMove2(cell.head, subMoveable) is Deny => tail
          case _                                                        => cell
        }
      val indexF =
        (if (cell.head._1 eq focus) \/-(0) else -\/("")) orElse findFocusIndexA(cell2.whole)(_._1)
      (cell2, indexF)
    }

    for {
      pos                ← focusPos
      tr                 ← rowAtPos(pos)
      cellRes1           ← needNev(rowContentsIterator(tr).filter(_._2._1 ==* pos.cell), "empty cell")
      (cellRes2, indexF) = excludeOuter(cellRes1)
      result             ← Option.when(cellRes2.length > 1)(leftRight.moveNevF(cellRes2, indexF)).sequence
    } yield result.map(t => TableCellZipper(t._1))
  }

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
