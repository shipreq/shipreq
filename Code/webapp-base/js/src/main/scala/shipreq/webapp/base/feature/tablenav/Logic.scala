package shipreq.webapp.base.feature.tablenav

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.ReactExt_DomNode
import japgolly.univeq._
import org.scalajs.dom.{ClientRect, html}
import scala.annotation.tailrec
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.{Deny, Permission}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.lib.DomUtil._

private[tablenav] object Logic {

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
            cellContentsIterator(td._1, false).find(_._1 eq focus) match {
              case Some((_, subPos)) => \/-(subPos)
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

  def movableRowIterator(table: html.Table): Iterator[html.TableRow] =
    table
      .children.iterator
      .filter(t => t.tagName ==* "TBODY" || t.tagName ==* "THEAD")
      .flatMap(_.domAsHtml
        .children.iterator
        .filter(_.tagName ==* "TR")
        .map(_.domCast[html.TableRow])
        .filter(rowMovableElementsIterator(_).nonEmpty))

  def rowMovableElementsIterator(tr: html.Element): Iterator[html.Element] =
    (0 until tr.children.length).iterator.flatMap { i =>
      val cell = tr.children(i).domAsHtml
      val children = focusableChildren(cell).filter(allowMove)
      if (children.nonEmpty)
        children
      else if (isFocusable(cell))
        cell :: Nil
      else
        Nil
    }

  type RowContent = (html.Element, (Int, Option[PosXY]))

  def rowContentsIterator(tr: html.Element): Iterator[RowContent] =
    (0 until tr.children.length).iterator.flatMap { i =>
      val h = tr.children(i).domAsHtml
      cellContentsIterator(h, true).map(_.map2(s => (i, s)))
    }

  def cellContentsIterator(cell: html.Element, includeSelf: Boolean): Iterator[(html.Element, Option[PosXY])] = {
    val it: Iterator[(html.Element, Option[PosXY])] =
      focusableChildren(cell)
        .zipWithIndex
        .map { case (e, j) => e -> Some(PosXY(j, 0)) }
    if (includeSelf && isFocusable(cell))
      Iterator.single(cell -> None) ++ it
    else
      it
  }

  def indexWhereF[A](as: IndexedSeq[A])(f: A => Boolean, err: => String): F[Int] = {
    val i = as.indexWhere(f)
    if (i < 0)
      -\/(err)
    else
      \/-(i)
  }

  /*
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
  */

  // PosXY#Y component isn't used right now, so only the X-axis is needed
  def distanceRect(a: ClientRect): ClientRect => Double =
    b => Math.abs((a.left + a.width / 2) - (b.left + b.width / 2))

  def focusClosest(focus: html.Element, candidates: Iterator[html.Element]): Option[TableCellZipper] = {
    // Note: If I ever add logic to start making use of the Y in PosXY this logic will need some
    // adjustment. It doesn't consider height wrapping. If the user presses down in the last row and the
    // top row cells have sub-items with PosXY.y > 0 the distance fn will rank the bottom ones is being
    // closer to the currentFocus which doesn't make sense; in such a case the user would expect pressing
    // down from the very bottom will go the very top, not the bottom of the top-most row.
    val distRectFn = distanceRect(focus.getBoundingClientRect())
    val closest = candidates
      .filter(allowMove)
      .minOptionBy(e => distRectFn(e.getBoundingClientRect()))
    closest.map(TableCellZipper(_))
  }

  /** Special-case: When a table cell contains sub-items that are focusable and satisfy allowMove (i.e. can be accessed
    * via arrow keys, not just tab), then the table cell itself should be excluded from move consideration.
    *
    * Examples:
    *
    * - A cell is focusable and has nothing but plain text in it, focus the cell when the user navigates there.
    *
    * - If it's got a checkbox in it, bypass the cell and focus the checkbox directly.
    *
    * - In the case of the implications field on the ReqDetail table, there are two divs that can be turned into editors,
    * focus those divs directly.
    *
    * - In the case of use case steps fields, if there are steps, then focus the steps themselves, not the surrounding
    * cell.
    */
  def allowMove2(a: RowContent, ob: Option[RowContent]): Permission =
    allowMove2A(a, ob)(_._2._1, _._2._2)

  def allowMove2A[A](a: A, ob: Option[A])(cell: A => Int, sub: A => Option[PosXY]): Permission =
    Deny.when(sub(a).isEmpty && ob.exists(b => sub(b).isDefined && cell(a) ==* cell(b)))

  def needNev[A](as: TraversableOnce[A], err: => String): F[NonEmptyVector[A]] =
    NonEmptyVector.maybe[A, F[NonEmptyVector[A]]](as.toVector, -\/(err))(\/-(_))

  def subAt(cell: html.Element, xy: PosXY): Option[html.Element] =
    cellContentsIterator(cell, false)
      .find(_._2.exists(_ ==* xy))
      .map(_._1)

}
