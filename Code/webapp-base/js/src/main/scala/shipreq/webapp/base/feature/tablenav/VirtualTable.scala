package shipreq.webapp.base.feature.tablenav

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import org.scalajs.dom.html
import org.scalajs.dom.raw
import scala.scalajs.js
import shipreq.webapp.base.lib.DomUtil._
import Logic.HtmlElementExtX

// NOTE: This only handles rowSpans atm because it's all I need and simpler to handle in isolation

private[tablenav] trait VirtualTable {
  val root: html.Table

  def sections: Int
  def virtualRows(section: Int): Int
  def virtualCols(section: Int, row: Int): Int

  def virtualPos(section: Int, pos: RealPos): VirtualPos
  def realPos(section: Int, pos: VirtualPos): RealPos

  final def virtualLoc(loc: RealLoc): VirtualLoc =
    VirtualLoc(loc.section, virtualPos(loc.section, loc.pos), loc.sub)

  final def realLoc(loc: VirtualLoc): RealLoc =
    RealLoc(loc.section, realPos(loc.section, loc.pos), loc.sub)

  def isRoot(section: Int, pos: VirtualPos): Boolean

  final def isRoot(loc: VirtualLoc): Boolean =
    isRoot(loc.section, loc.pos)

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

  final def virtualCols(loc: VirtualLoc): Int =
    virtualCols(loc.section, loc.row)
}

private[tablenav] object VirtualTable {

  def apply(t: html.Table): VirtualTable =
    new VirtualTable {

      private val s = t.children.iterator.map(Section).toArray

      override val root = t

      override def sections = s.length

      override def virtualRows(section: Int): Int =
        s(section).virtualRows

      override def virtualCols(section: Int, row: Int): Int =
        s(section).virtualCols(row)

      override def virtualPos(section: Int, pos: RealPos): VirtualPos =
        s(section).toVirtual(pos)

      override def realPos(section: Int, pos: VirtualPos): RealPos =
        s(section).toReal(pos)

      override def isRoot(section: Int, pos: VirtualPos): Boolean =
        s(section).isRoot(pos)
    }

  def oneToOne(t: html.Table): VirtualTable =
    new VirtualTable {

      override val root = t

      override def sections = t.children.length

      override def virtualRows(section: Int): Int =
        t.children(section).children.length

      override def virtualCols(section: Int, row: Int): Int =
        t.children(section).children(row).children.length

      override def virtualPos(section: Int, pos: RealPos): VirtualPos =
        VirtualPos(pos.tr, pos.child)

      override def realPos(section: Int, pos: VirtualPos): RealPos =
        RealPos(pos.row, pos.col)

      override def isRoot(section: Int, pos: VirtualPos): Boolean =
        true
    }

  // -------------------------------------------------------------------------------------------------------------------

  private final case class Section(dom: raw.Element) {

    private val rows = {
      val array = new js.Array[RealRow]
      for (i <- dom.children.indices) {
        val row = dom.children(i).domCast[html.TableRow]
        val prev = Option.unless(i == 0)(array(i - 1))
        val rr = RealRow(prev, i, row)
        array.push(rr)
      }
      array
    }

    def toVirtual(pos: RealPos): VirtualPos =
      rows(pos.tr).toVirtual(pos.child)

    def toReal(pos: VirtualPos): RealPos =
      rows(pos.row).toReal(pos.col)

    def isRoot(pos: VirtualPos): Boolean =
      rows(pos.row).isRoot(pos.col)

     def virtualRows: Int =
       rows.length

     def virtualCols(row: Int): Int =
       rows(row).virtualCols
  }

  // -------------------------------------------------------------------------------------------------------------------

  private final case class RealRow(prev: Option[RealRow], realRow:Int, row: html.TableRow) {

    private val prevCells: js.Array[Cell] = prev.fold(new js.Array[Cell])(_.cells)
    private val cells = new js.Array[Cell]
    private val realChildVCols = new js.Array[Int]

    val it = row.children.iterator.map(_.domCast[html.TableCell])

    def go(col: Int): Unit = {
      val colOk = col < prevCells.length
      if (colOk || it.hasNext) {
        val prevCell = if (colOk) Some(prevCells(col)) else None
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
              throw new RuntimeException("rowSpan≤0")
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
          assert(col == cell.vcol, "col == cell.vcol")
          cells.push(cell match {
            case r: Cell.Root => Cell.Cont(r, col, 1, r.nextRows - 1)
            case c: Cell.Cont => Cell.Cont(c.root, col, c.prevRows + 1, c.nextRows - 1)
          })
          go(col + 1)
        }
      }
    }
    go(0)

    def toVirtual(realChild: Int): VirtualPos = {
//      println(s"===================================================================================================")
//      println(s"input cell = $cell")
////      println(s"cols       = ${cols.map(i => if (i == null) "null" else i.innerText).mkString("[", ",","]")}")
//      println(s"realRow    = ${realRow}")
//      println(s"prev       = ${prevCells.mkString("[", ", ","]")}")
//      println(s"cells      = ${cells.mkString("[", ", ","]")}")
//      println(s"reals      = ${reals.mkString("[", ",","]")}")
      val vcol = realChildVCols(realChild)
      val c = cells(vcol)
      VirtualPos(realRow, c.vcol)
    }

    def toReal(col: Int): RealPos = {
      val c = cells(col).root
      RealPos(c.row, c.realChild)
    }

    def isRoot(col: Int): Boolean =
      cells(col) match {
        case _: Cell.Root => true
        case _: Cell.Cont => false
      }

    def virtualCols: Int =
      cells.length
  }

  // -------------------------------------------------------------------------------------------------------------------

  sealed trait Cell {
    def root: Cell.Root
    val vcol: Int
    val nextRows: Int
  }

  object Cell {
    final case class Root(row: Int, realChild: Int, vcol: Int, nextRows: Int) extends Cell {
      override def root = this
    }
    final case class Cont(root: Root, vcol: Int, prevRows: Int, nextRows: Int) extends Cell
  }

}
