package shipreq.webapp.base.feature.tablenav

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import nyaya.util.Multimap
import org.scalajs.dom.html
import scalaz.std.option.optionInstance
import scalaz.syntax.traverse._
import scalaz.{-\/, \/-}
import shipreq.base.util.Deny
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.lib.DomUtil._

object TableCellZipper {

  def apply(focus: html.Element)(implicit ts: TableStyle): TableCellZipper =
    new TableCellZipper(focus)

  def option(o: Option[html.Element])(implicit ts: TableStyle): F[TableCellZipper] =
    o match {
      case Some(e) => \/-(apply(e))
      case None    => -\/("Nothing focusable")
    }

  def within(e: html.Element)(implicit ts: TableStyle): F[TableCellZipper] =
    if (isFocusable(e))
      \/-(apply(e))
    else
      option(focusableChildren(e).nextOption())
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final class TableCellZipper(val focus: html.Element)(implicit tableStyle: TableStyle) {
  import Ordering.Double.TotalOrdering
  import Logic._

  private lazy val rootAndPos =
    findRootAndPos(parentsAndIndices(focus), focus)

  def virtualTable: F[VirtualTable] =
    rootAndPos.map(_._1)

  def focusVLoc: F[VirtualLoc] =
    rootAndPos.map(_._2)

  def move(a: Axis, om: Option[Movement]): F[TableCellZipper] =
    om match {
      case None    => \/-(this)
      case Some(m) => move(a, m)
    }

  def move(a: Axis, m: Movement): F[TableCellZipper] =
    a match {
      case Axis.UpDown    => moveUpDown(m)
      case Axis.LeftRight => moveLeftRight(m)
    }

  private def moveUpDown(m: Movement): F[TableCellZipper] = {
    type Sub = (html.Element, Option[PosXY])

    def virtualRows(vt: VirtualTable, focusLoc: VirtualLoc): Iterator[(Int, Int, NonEmptyVector[Sub])] =
      (0 until vt.sectionCount).iterator
        .flatMap(section => (0 until vt.virtualRowCount(section)).iterator.map((section, _)))
        .flatMap { case (section, row) =>

          def cells: Iterator[html.Element] = {
            val len   = vt.virtualColCount(section, row)
            val first = focusLoc.col.min(len - 1)
            val it    = (first to 0 by -1).iterator ++ (first + 1 until len)
            if (vt.isRootCell(section, VirtualPos(row, first)))
              it
                .map(col => VirtualLoc(section, VirtualPos(row, col), None))
                .map(loc => vt.cellAt(loc).toOption)
                .filterDefined
            else
              Iterator.empty
          }

          //                       cell     rows     subcells
          def tableCellsInReverse: Iterator[Iterator[NonEmptyVector[Sub]]] =
            cells.map { cell =>
              var mm0 = Option.empty[Sub]
              var mm = Multimap.empty[Int, Vector, Sub]
              cellContentsIterator(cell, true)
                .foreach(i => if (allowMove(i._1))
                  i._2 match {
                    case None     => mm0 = Some(i)
                    case Some(xy) => mm = mm.add(xy.y, i)
                  }
                )
              if (mm.isEmpty)
                mm0 match {
                  case Some(s) => Iterator single NonEmptyVector.one(s)
                  case None    => Iterator.empty
                }
              else
                MutableArray(mm.keys).sort.iterator.map(NonEmptyVector force mm(_))
            }

          tableCellsInReverse
            .filter(_.nonEmpty) // remove empty cells
            .take(1)
            .flatMap(_.map((section, row, _)))
        }

    def focusClosestX[A](candidates: NonEmptyVector[Sub]): TableCellZipper = {
      val distRectFn = distanceRectX(focus.getBoundingClientRect())
      val closest = candidates.whole.minBy(s => distRectFn(s._1.getBoundingClientRect()))
      TableCellZipper(closest._1)
    }

    for {
      vt        <- virtualTable
      curLoc    <- focusVLoc
      rows      <- needNev(virtualRows(vt, curLoc), "no rows")
      isCur     = (x: (Int, Int, Any)) => (x._1 == curLoc.section) && (x._2 == curLoc.row)
      rowSubIdx = rows.whole.indexWhere(x => isCur(x) && x._3.exists(_._1 eq focus))
      rowIdx    <- indexWhereF(rows.whole)(isCur, "Focus row not found")
    } yield {

      def move(rowIdx: Int): TableCellZipper = {
        val target = m.moveNev(rows, rowIdx)._3
        curLoc.sub match {
          case None =>
            TableCellZipper(target.head._1)
          case Some(xy) =>
            target.find(_._2.exists(_.x ==* xy.x)) match {
              case Some((sub, _)) => TableCellZipper(sub)
              case None           => focusClosestX(target)
            }
        }
      }

      if (rowSubIdx >= 0)
        move(rowSubIdx)
      else m match {
        case Movement.Next => TableCellZipper(rows.unsafeApply(rowIdx)._3.head._1)
        case _             => move(rowIdx)
      }
    }
  }

  private def moveLeftRight(m: Movement): F[TableCellZipper] = {
    def candidates(vt: VirtualTable, focusLoc: VirtualLoc): Vector[RowContent] = {
      val list = rowContentsIterator(vt, focusLoc).filter(_._1 |> allowMove).toList

      // Determine appropriate sub-rows per cell
      val yPerCell: Map[Int, Int] =
        list.groupBy(_._2._1).mapValuesNow(
          focusLoc.sub match {

            case None =>
              vs =>
                if (vs.exists(_._2._2.isDefined))
                  0 // Choose the top-most sub-row
                else
                  -1

            case Some(xy) =>
              vs =>
                if (vs.head._2._1 ==* focusLoc.col)
                  xy.y // Stay on current sub-row in current cell
                else
                // Choose same sub-row in other cells, or the bottom if there are less
                  vs.iterator.map(_._2._2.fold(-1)(_.y)).max min xy.y
          }
        )

      list.iterator.filter { i =>
        val selY = yPerCell(i._2._1)
        if (selY ==* -1)
          i._2._2.isEmpty
        else
          i._2._2.exists(_.y ==* selY)
      }.toVector
    }

    for {
      vt   <- virtualTable
      vloc <- focusVLoc
      rowResults <- needNev(candidates(vt, vloc), "no LR candidates")
      indexF     = findFocusIndexA(rowResults.whole)(_._1)
      target     <- m.moveNevF(rowResults, indexF)
    } yield TableCellZipper(target._1)
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
      vt                 <- virtualTable
      vloc               <- focusVLoc
      cellRes1           <- needNev(rowContentsIterator(vt, vloc).filter(_._2._1 ==* vloc.col), "empty cell")
      (cellRes2, indexF) = excludeOuter(cellRes1)
      result             <- Option.when(cellRes2.length > 1)(leftRight.moveNevF(cellRes2, indexF)).sequence
    } yield result.map(t => TableCellZipper(t._1))
  }

  def goto(loc: VirtualLoc): F[TableCellZipper] =
    for {
      vt     <- virtualTable
      td     <- vt.cellAt(loc)
      result <- loc.sub match {
        case None    => \/-(TableCellZipper(td))
        case Some(s) =>
          cellContentsIterator(td, false).find(_._2.exists(_ ==* s)) match {
            case Some((e, _)) => \/-(TableCellZipper(e))
            case None         => -\/(s"Nothing at $s")
          }
      }
    } yield result

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private def findFocusIndexA[A](as: IndexedSeq[A])(element: A => html.Element): F[Int] =
    indexWhereF(as)(element(_) eq focus, s"Focus not found: ${focus.outerHTML.take(255)}")
}
