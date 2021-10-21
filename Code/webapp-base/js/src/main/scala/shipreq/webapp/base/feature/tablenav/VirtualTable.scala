package shipreq.webapp.base.feature.tablenav

import japgolly.scalajs.react._
import org.scalajs.dom.{Element, html}
import scala.scalajs.js
import shipreq.webapp.base.feature.tablenav.Logic.HtmlElementExtX

// NOTE: This only handles rowSpans atm because it's all I need and simpler to handle in isolation

trait VirtualTable {

  val root: html.Table
  def sectionCount: Int
  def virtualRowCount(section: Int): Int
  def virtualColCount(section: Int, row: Int): Int
  def virtualPos(section: Int, pos: RealPos): VirtualPos
  def realPos(section: Int, pos: VirtualPos): RealPos
  def isRootCell(section: Int, pos: VirtualPos): Boolean

  final def virtualLoc(loc: RealLoc): VirtualLoc =
    VirtualLoc(loc.section, virtualPos(loc.section, loc.pos), loc.sub)

  final def realLoc(loc: VirtualLoc): RealLoc =
    RealLoc(loc.section, realPos(loc.section, loc.pos), loc.sub)

  final def isRootCell(loc: VirtualLoc): Boolean =
    isRootCell(loc.section, loc.pos)

  final def rowAt(loc: RealLoc): F[html.TableRow] =
    for {
      tbody <- root.child(loc.section)
      tr    <- tbody.child(loc.tr)
    } yield tr.domCast[html.TableRow]

  /** Ignores sub-pos */
  final def cellAt(loc: RealLoc): F[html.TableCell] =
    for {
      tr <- rowAt(loc)
      td <- tr.child(loc.child)
    } yield td.domCast[html.TableCell]

  final def rowAt(loc: VirtualLoc): F[html.TableRow] =
    rowAt(realLoc(loc))

  /** Ignores sub-pos */
  final def cellAt(loc: VirtualLoc): F[html.TableCell] =
    cellAt(realLoc(loc))

  final def virtualColCount(loc: VirtualLoc): Int =
    virtualColCount(loc.section, loc.row)
}

object VirtualTable {

  def apply(t: html.Table)(implicit ts: TableStyle): VirtualTable =
    if (ts.hasRowSpans)
      from(t)
    else
      oneToOne(t)

  def from(t: html.Table): VirtualTable =
    new VirtualTable {

      private val s = t.children.iterator.map(Section).toArray

      override val root = t

      override def sectionCount = s.length

      override def virtualRowCount(section: Int): Int =
        s(section).virtualRowCount

      override def virtualColCount(section: Int, row: Int): Int =
        s(section).virtualColCount(row)

      override def virtualPos(section: Int, pos: RealPos): VirtualPos =
        s(section).virtualPos(pos)

      override def realPos(section: Int, pos: VirtualPos): RealPos =
        s(section).realPos(pos)

      override def isRootCell(section: Int, pos: VirtualPos): Boolean =
        s(section).isRootCell(pos)
    }

  def oneToOne(t: html.Table): VirtualTable =
    new VirtualTable {

      override val root = t

      override def sectionCount = t.children.length

      override def virtualRowCount(section: Int): Int =
        t.children(section).children.length

      override def virtualColCount(section: Int, row: Int): Int =
        t.children(section).children(row).children.length

      override def virtualPos(section: Int, pos: RealPos): VirtualPos =
        VirtualPos(pos.tr, pos.child)

      override def realPos(section: Int, pos: VirtualPos): RealPos =
        RealPos(pos.row, pos.col)

      override def isRootCell(section: Int, pos: VirtualPos): Boolean =
        true
    }

  // -------------------------------------------------------------------------------------------------------------------

  private final case class Section(dom: Element) {

    private val rows = {
      val array = new js.Array[RealRow]
      for (i <- dom.children.indices) {
        val row  = dom.children(i).domCast[html.TableRow]
        val prev = Option.unless(i == 0)(array(i - 1))
        val rr   = RealRow(prev, i, row)
        array.push(rr)
      }
      array
    }

    def virtualPos(pos: RealPos): VirtualPos =
      rows(pos.tr).virtualPos(pos.child)

    def realPos(pos: VirtualPos): RealPos =
      rows(pos.row).realPos(pos.col)

    def isRootCell(pos: VirtualPos): Boolean =
      rows(pos.row).isRootCell(pos.col)

     def virtualRowCount: Int =
       rows.length

     def virtualColCount(row: Int): Int =
       rows(row).virtualColCount
  }

  // -------------------------------------------------------------------------------------------------------------------

  private final case class RealRow(prev: Option[RealRow], realRow:Int, row: html.TableRow) {

    private val prevCells: js.Array[Cell] = prev.fold(new js.Array[Cell])(_.cells)
    private val cells = new js.Array[Cell]
    private val realChildVCols = new js.Array[Int]

    private def init(): Unit = {
      val it = row.children.iterator.map(_.domCast[html.TableCell])
      @tailrec def go(col: Int): Unit = {
        val colOk = col < prevCells.length
        if (colOk || it.hasNext) {
          val prevCell = Option.when(colOk)(prevCells(col))
          val noneOrDone = prevCell.forall(_.nextRows == 0)
          if (noneOrDone) {
            if (it.hasNext) {
              val td = it.next()
              val rowSpan = td.rowSpan
              if (rowSpan > 0) {
                cells.push(Cell.Root(realRow, realChildVCols.length, col, rowSpan - 1))
                realChildVCols.push(col)
                go(col + 1)
              } else
                throw new RuntimeException("rowSpan≤0") // not supported
            } else {
              // it.isEmpty
              if (prevCell.isDefined) {
                go(col + 1)
              } else
                () // done
            }
          } else {
            // continue from previous row
            val cell = prevCell.get
            assert(col == cell.col, "col == cell.col")
            cells.push(cell match {
              case r: Cell.Root => Cell.Cont(r, col, 1, r.nextRows - 1)
              case c: Cell.Cont => Cell.Cont(c.root, col, c.prevRows + 1, c.nextRows - 1)
            })
            go(col + 1)
          }
        }
      }
      go(0)
    }

    init()

    def virtualPos(realChild: Int): VirtualPos = {
      val vcol = realChildVCols(realChild)
      val c = cells(vcol)
      VirtualPos(realRow, c.col)
    }

    def realPos(col: Int): RealPos = {
      val c = cells(col).root
      RealPos(c.row, c.realChild)
    }

    def isRootCell(col: Int): Boolean =
      cells(col) match {
        case _: Cell.Root => true
        case _: Cell.Cont => false
      }

    def virtualColCount: Int =
      cells.length
  }

  // -------------------------------------------------------------------------------------------------------------------

  sealed trait Cell {
    def root: Cell.Root
    val col: Int
    val nextRows: Int
  }

  object Cell {
    final case class Root(row: Int, realChild: Int, col: Int, nextRows: Int) extends Cell {
      override def root = this
    }
    final case class Cont(root: Root, col: Int, prevRows: Int, nextRows: Int) extends Cell
  }

}
