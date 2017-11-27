package shipreq.webapp.base.feature


import japgolly.univeq._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.{raw => _, _}
import scala.annotation.tailrec
import scalajs.js
import org.scalajs.dom.{Element, console, html}
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.Util
import shipreq.webapp.base.lib.DomUtil.{TableCellZipper => _, _}

object TableNavigationFeature {

  final case class PosXY(x: Int, y: Int)
  object PosXY {
    implicit def univEq: UnivEq[PosXY] = UnivEq.derive
  }

  final case class TablePos(body: Int, row: Int, cell: Int, sub: Option[PosXY])
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

    private def cellContentsIterator(cell: html.Element): Iterator[(html.Element, PosXY)] =
      cell.querySelectorAll("input")
        .iterator
        .map(_.domCast[html.Input])
        .filter(i => i.`type` == "checkbox")
        .focusable
        .zipWithIndex
        .map { case (e, j) => e -> PosXY(j, 0) }

    def move(a: Axis, m: Movement): F[TableCellZipper] =
      focusPos.flatMap(pos =>
        a match {
          case Axis.LeftRight =>
            for {
              table <- root
              tbody <- table.child(pos.body)
              tr    <- tbody.child(pos.row)
            } yield {

              val rowResuts: Vector[(html.Element, TablePos)] =
                (0 until tr.children.length).iterator.flatMap { i =>
                  val h = tr.children(i).domAsHtml
                  var rs: List[(html.Element, TablePos)] =
                    cellContentsIterator(h)
                      .map(_.map2(s => pos.copy(cell = i, sub = Some(s))))
                      .toList
                  if (isFocusable(h))
                    rs = (h, pos.copy(cell = i, sub = None)) :: rs
                  rs
                }.toVector

              val i = rowResuts.indexWhere(_._1 eq focus)

//              if (pos == TablePos(0,4,5,Some(PosXY(0,0)))) {
//                println(s"\n\n${rowResuts.zipWithIndex.mkString("\n")}\ni = $i\n")
//              }

              if (i < 0) {
                console.warn(s"$pos not found in $rowResuts")
                this
              } else {
                val j = Util.fitCollectionIndex(m adjustIndex i, rowResuts.length)
                val (e2, pos2) = rowResuts(j)
                // println(s"$a $m $i --> $j = ${e.outerHTML}")
                TableCellZipper(e2)
              }
            }
        }
      )

  }
}
