package shipreq.webapp.base.feature


import japgolly.univeq._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.{raw => _, _}
import scala.annotation.tailrec
import scalajs.js
import org.scalajs.dom.{Element, console, html}
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.std.option._
import shipreq.base.util.Util
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

  type ParentStream = Stream[(html.Element, Int)]

  def parentsAndIndices(e: html.Element): ParentStream =
    Option(e.parentElement) match {
      case Some(p) => Stream((e, siblingIndex(e))) append parentsAndIndices(p)
      case None    => Stream((e, 0))
    }

  private implicit class HtmlElementExtX(private val e: html.Element) extends AnyVal {
    def child(i: Int): String \/ html.Element =
      if (e.children.isEmpty)
        -\/(s"No children found: ${e.outerHTML}")
      else if (i >= e.children.length)
        -\/(s"No child at $i")
      else
        \/-(e.children(i).domAsHtml)
//      else {
//        val j = Util.fitCollectionIndex(i, e.children.length)
//        \/-(e.children(j).domAsHtml)
//      }
  }

  final case class TableCellZipper(focus: html.Element) {
    //  @inline implicit private def autoCastHtml(e: Element) = e.domAsHtml

    type F[A] = String \/ A

    private lazy val parentStream =
      parentsAndIndices(focus)

    private lazy val stuff =
      findStuff(0)

    def root: F[html.Table] =
      stuff.map(_._1)

    def focusPos: F[TablePos] =
      stuff.map(_._2)

    def goto(pos: TablePos): F[TableCellZipper] =
      for {
        table <- root
        tbody <- table.child(pos.body)
        tr    <- tbody.child(pos.row)
        td    <- tr   .child(pos.cell)
        result <- pos.sub match {
          case None    => \/-(TableCellZipper(td))
          case Some(s) =>
            cellContentsIterator(td).find(_._2 ==* s) match {
              case Some((e, _)) => \/-(TableCellZipper(e))
              case None         => -\/(s"Nothing at $s")
            }
        }
      } yield result

    def move(a: Axis, m: Movement): F[TableCellZipper] =
      focusPos.flatMap(pos =>
        a match {
          case Axis.LeftRight =>
            for {
              tr        <- cellAtSuperPos(pos)
              rowResults = rowContentsIterator(tr, pos).toVector
              i         <- findFocusIndex(rowResults)(_._1)
            } yield _move(m, i, rowResults)(_._1)
        }
      )

    /** Move horizontally within the same cell, if there is somewhere to move to.
      *
      * This is usually tab/shift-tab in table cells so users can jump in/out of text editors.
      * The reason that up/down/left/right doesn't automatically enter text editors is that those keys are used to
      * navigate the text itself inside the editor.
      * */
    def subMove(leftRight: Movement): F[Option[TableCellZipper]] =
      focusPos.flatMap(pos =>
        for {
          tr         <- cellAtSuperPos(pos)
          superPos    = pos.withoutSub
          cellResults = rowContentsIterator(tr, pos).filter(_._2.withoutSub ==* superPos).toVector
          i          <- findFocusIndex(cellResults)(_._1)
        } yield
          Option.when(cellResults.length > 1)(
            _move(leftRight, i, cellResults)(_._1))
      )

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    private def findStuff(innerElements: Int): F[(html.Table, TablePos)] = {
      val parentStream2 = parentStream.drop(innerElements)
      parentStream2.map(_._1.tagName) match {

        case ("TD" | "TH") #:: "TR" #:: ("TBODY" | "THEAD") #:: "TABLE" #:: _ =>
          val (td #:: tr #:: tbody #:: table #:: _) = parentStream2
          val subAttempt: String \/ Option[PosXY] =
            if (innerElements ==* 0)
              \/-(None)
            else
              cellContentsIterator(td._1).find(_._1 eq focus) match {
                case Some((_, subPos)) => \/-(Some(subPos))
                case None              =>
                  //                  println("="*120)
                  //                  println(td._1.outerHTML)
                  //                  println(parentStream.drop(innerElements).map(_._1.tagName).mkString(", "))
                  //                  println("="*120)
                  //                  cellContentsIterator(td._1).foreach(println)
                  //                  println("="*120)
                  -\/("Unable to determine subpos of " + focus.outerHTML.take(100))
              }

          subAttempt.map(sub =>
            (table._1.domCast[html.Table], TablePos(tbody._2, tr._2, td._2, sub)))

        case _ #:: _ =>
          findStuff(innerElements + 1)

        case Stream.Empty =>
          -\/("Unable to determine table structure")
      }
    }

    /** Ignores sub-pos */
    private def cellAtSuperPos(pos: TablePos): F[html.Element] =
      for {
        table <- root
        tbody <- table.child(pos.body)
        tr    <- tbody.child(pos.row)
      } yield tr

    private def rowContentsIterator(tr: html.Element, pos: TablePos): Iterator[(html.Element, TablePos)] =
      (0 until tr.children.length).iterator.flatMap { i =>
        val h = tr.children(i).domAsHtml
        var rs: List[(html.Element, TablePos)] =
          cellContentsIterator(h)
            .map(_.map2(s => pos.copy(cell = i, sub = Some(s))))
            .toList
        if (isFocusable(h))
          rs = (h, pos.copy(cell = i, sub = None)) :: rs
        rs
      }

    private def cellContentsIterator(cell: html.Element): Iterator[(html.Element, PosXY)] =
      cell.querySelectorAll("input")
        .iterator
        .map(_.domCast[html.Input])
        .filter(i => i.`type` == "checkbox")
        .focusable
        .zipWithIndex
        .map { case (e, j) => e -> PosXY(j, 0) }

//    private def findFocus[A](as: TraversableOnce[A])(element: A => html.Element): F[A] =
//      as.find(element(_) eq focus) \/> "Focus not found"
//
//    private def findFocusAndIndex[A](as: TraversableOnce[A])(element: A => html.Element): F[(A, Int)] =
//      findFocus(as.toIterator.zipWithIndex)(x => element(x._1))

    private def findFocusIndex[A](as: IndexedSeq[A])(element: A => html.Element): F[Int] = {
      val i = as.indexWhere(element(_) eq focus)
      if (i < 0)
        -\/("Focus not found")
      else
        \/-(i)
    }

    private def _move[A](m: Movement, i: Int, as: IndexedSeq[A])(element: A => html.Element) = {
      val j = Util.fitCollectionIndex(m adjustIndex i, as.length)
      val e2 = element(as(j))
      TableCellZipper(e2)
    }
  }
}
