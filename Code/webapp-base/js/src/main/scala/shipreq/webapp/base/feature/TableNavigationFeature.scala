package shipreq.webapp.base.feature


import japgolly.univeq._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.{raw => _, _}
import scala.annotation.tailrec
import scalajs.js
import org.scalajs.dom.{ClientRect, console, html}
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.std.option._
import shipreq.base.util.{Identity, Util}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.lib.DomUtil.{TableCellZipper => _, _}

object TableNavigationFeature {

  final case class PosXY(x: Int, y: Int)
  object PosXY {
    implicit def univEq: UnivEq[PosXY] = UnivEq.derive
  }

  final case class TablePos(body: Int, row: Int, cell: Int, sub: Option[PosXY]) {
    def withoutSub: TablePos =
      copy(sub = None)
  }
  object TablePos {
    implicit def univEq: UnivEq[TablePos] = UnivEq.derive
  }

  sealed trait Axis
  object Axis {
    case object UpDown extends Axis
    case object LeftRight extends Axis
  }

  type F[A] = String \/ A

  private object Internals {

    type ParentStream = Stream[(html.Element, Int)]

    def parentsAndIndices(e: html.Element): ParentStream =
      Option(e.parentElement) match {
        case Some(p) => Stream((e, siblingIndex(e))) append parentsAndIndices(p)
        case None    => Stream((e, 0))
      }

    implicit class HtmlElementExtX(private val e: html.Element) extends AnyVal {
      def child(i: Int): String \/ html.Element =
        if (e.children.isEmpty)
          -\/(s"No children found: ${e.outerHTML}")
        else if (i >= e.children.length)
          -\/(s"No child at $i")
        else
          \/-(e.children(i).domAsHtml)
    }

    @tailrec
    def findRootAndPos(parentStream: ParentStream, sub: Boolean, focus: html.Element): F[(html.Table, TablePos)] =
      parentStream.map(_._1.tagName) match {

        case ("TD" | "TH") #:: "TR" #:: ("TBODY" | "THEAD") #:: "TABLE" #:: _ =>
          val (td #:: tr #:: tbody #:: table #:: _) = parentStream
          val subAttempt: String \/ Option[PosXY] =
            if (sub)
              cellContentsIterator(td._1).find(_._1 eq focus) match {
                case Some((_, subPos)) => \/-(Some(subPos))
                case None              =>
                  // println("="*120)
                  // println(td._1.outerHTML)
                  // println(parentStream.drop(innerElements).map(_._1.tagName).mkString(", "))
                  // println("="*120)
                  // cellContentsIterator(td._1).foreach(println)
                  // println("="*120)
                  -\/("Unable to determine subpos of " + focus.outerHTML.take(100))
              }
            else
              \/-(None)

          subAttempt.map(sub =>
            (table._1.domCast[html.Table], TablePos(tbody._2, tr._2, td._2, sub)))

        case _ #:: _ =>
          findRootAndPos(parentStream.tail, true, focus)

        case Stream.Empty =>
          -\/("Unable to determine table structure")
      }

    def rowContentsIterator(tr: html.Element): Iterator[(html.Element, (Int, Option[PosXY]))] =
      (0 until tr.children.length).iterator.flatMap { i =>
        val h = tr.children(i).domAsHtml
        var rs: List[(html.Element, (Int, Option[PosXY]))] =
          cellContentsIterator(h)
            .map(_.map2(s => (i, Some(s))))
            .toList
        if (isFocusable(h))
          rs = (h, (i, None)) :: rs
        rs
      }

    /** excludes argument from results */
    def cellContentsIterator(cell: html.Element): Iterator[(html.Element, PosXY)] =
      cell.children.deepIteratorDepthFirst
        .asHtml
        .focusable
        .zipWithIndex
        .map { case (e, j) => e -> PosXY(j, 0) }

//    def _move(m: Movement, i: Int, as: IndexedSeq[html.Element]) =
//      _moveA(m, i, as)(Identity.apply)
//
//    def _moveA[A](m: Movement, i: Int, as: IndexedSeq[A])(element: A => html.Element) = {
//      val j = Util.fitCollectionIndex(m adjustIndex i, as.length)
//      val e2 = element(as(j))
//      TableCellZipper(e2)
//    }
//
//    def __moveA[A](m: Movement, i: Int, as: IndexedSeq[A])(element: A => html.Element): A = {
//      val j = Util.fitCollectionIndex(m adjustIndex i, as.length)
//      element(as(j))
//    }

    def subMoveOnly: html.Element => Boolean = {
      case i: html.Input    => i.`type` ==* "text"
      case _: html.TextArea => true
      case _                => false
    }

    def indexWhereF[A](as: IndexedSeq[A])(f: A => Boolean, err: => String): F[Int] = {
      val i = as.indexWhere(f)
      if (i < 0)
        -\/(err)
      else
        \/-(i)
    }

    def hypotenuse(x: Double, y: Double): Double =
      Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2))

    def centerXY(r: ClientRect): (Double, Double) =
      (r.left + r.width / 2, r.top + r.height / 2)

    def distanceRect(a: ClientRect): ClientRect => Double = {
      val aa = centerXY(a)
      b => distanceXY(aa, centerXY(b))
    }

    def distanceXY(a: (Double, Double), b: (Double, Double)): Double =
      hypotenuse(a._1 - b._1, a._2 - b._2)
  }
  import Internals._

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  final case class TableCellZipper(focus: html.Element) {

    private lazy val rootAndPos =
      findRootAndPos(parentsAndIndices(focus), false, focus)

    def root: F[html.Table] =
      rootAndPos.map(_._1)

    def focusPos: F[TablePos] =
      rootAndPos.map(_._2)

    def move(a: Axis, m: Movement): F[TableCellZipper] =
        a match {
          case Axis.LeftRight =>
            for {
              pos        ← focusPos
              tr         ← rowAtPos(pos)
              rowResults = rowContentsIterator(tr).map(_._1).filterNot(subMoveOnly).toVector
              i          ← findFocusIndex(rowResults)
              // TODO i should be lazy, movement might ignore it
            } yield TableCellZipper(m(rowResults, i))

          case Axis.UpDown =>

            for {
              pos        ← focusPos
              table <- root
              tbody <- table.child(pos.body)
              tr    <- tbody.child(pos.row)
              td    <- tr.child(pos.cell)

              rows: Vector[html.TableRow] =
                table
                  .children.iterator
                  .filter(t => t.tagName ==* "TBODY" || t.tagName ==* "THEAD")
                  .flatMap(_.domAsHtml
                    .children.iterator
                    .filter(_.tagName ==* "TR")
                    .map(_.domCast[html.TableRow])
                    .filter(rowContentsIterator(_).nonEmpty))
                  .toVector

              rowIndex <- indexWhereF(rows)(_ eq tr, "Focus row not found")

            } yield {

              // TODO ↓ ignores moving up/down within the same cell ↓
              val newRow = m(rows, rowIndex)

              newRow.child(pos.cell) match {
                case \/-(cell) if isFocusable(cell) =>
                  TableCellZipper(cell)

                case _ =>
                  val distRectFn = distanceRect(focus.getBoundingClientRect())
                  // TODO closest doesn't consider wrap and movement dir
                  // eg. moving up from top should select furthest Y
                  val closest = rowContentsIterator(newRow)
                    .map(_._1)
                    .filterNot(subMoveOnly)
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

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
      indexWhereF(as)(element(_) eq focus, "Focus not found")

  }
}
