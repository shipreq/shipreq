package shipreq.webapp.base.feature.tablenav

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.ReactExt_DomNode
import org.scalajs.dom.{ClientRect, html}
import shipreq.base.util.{Deny, Permission}
import shipreq.webapp.base.lib.DomUtil._

object Attrs {
  val NestedTable = "data-tnf-nt"

  val NewRow = "data-tnf-nr"
}

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

  type ParentStream = LazyList[(html.Element, Int)]

  def parentsAndIndices(e: html.Element): ParentStream =
    Option(e.parentElement) match {
      case Some(p) => LazyList((e, siblingIndex(e))) #::: parentsAndIndices(p)
      case None    => LazyList((e, 0))
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
  def findRootAndPos(parentStream: ParentStream, focus: html.Element, expectSubPos: Boolean = false)
                    (implicit ts: TableStyle): F[(VirtualTable, VirtualLoc)] =
    parentStream.map(t => if (t._1.hasAttribute(Attrs.NestedTable)) "" else t._1.tagName) match {

      case ("TD" | "TH") #:: "TR" #:: ("TBODY" | "THEAD") #:: "TABLE" #:: _ =>

        val (td #:: tr #:: tbody #:: table #:: _) = parentStream

        val subAttempt: String \/ Option[PosXY] =
          if (expectSubPos)
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

        subAttempt.map { sub =>
          val tableDom = table._1.domCast[html.Table]
          val vt       = VirtualTable(tableDom)
          val section  = tbody._2
          val rpos     = RealPos(tr._2, td._2)
          val vpos     = vt.virtualPos(section, rpos)
          val vloc     = VirtualLoc(section, vpos, sub)
          (vt, vloc)
        }

      case _ #:: _ =>
        findRootAndPos(parentStream.tail, focus, expectSubPos = true)

      case _ =>
        -\/("Unable to determine table structure")
    }

  /** (Cell DOM, (virtual column, sub)) */
  type RowContent = (html.Element, (Int, Option[PosXY]))

  def rowContentsIterator(vt: VirtualTable, focusLoc: VirtualLoc): Iterator[RowContent] =
    (0 until vt.virtualColCount(focusLoc)).iterator
      .map(focusLoc.withCol)
      .map(l => vt.cellAt(l).toOption.map((_, l)))
      .filterDefined
      .flatMap { case (cell, loc) =>
        cellContentsIterator(cell, includeSelf = true)
          .map { case (dom, sub) => (dom, (loc.col, sub)) }
      }

  def cellContentsIterator(cell: html.Element, includeSelf: Boolean): Iterator[(html.Element, Option[PosXY])] = {
    var x = -1
    var y = 0
    val it: Iterator[(html.Element, Option[PosXY])] =
      focusableChildren(cell).map { e =>
        if (e.hasAttribute(Attrs.NewRow) && x != -1) {
          y += 1
          x = 0
        } else
          x += 1
        e -> Some(PosXY(x, y))
      }
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

  def distanceRectX(a: ClientRect): ClientRect => Double =
    b => Math.abs((a.left + a.width / 2) - (b.left + b.width / 2))

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

  def needNev[A](as: IterableOnce[A], err: => String): F[NonEmptyVector[A]] =
    NonEmptyVector.maybe[A, F[NonEmptyVector[A]]](as.iterator.toVector, -\/(err))(\/-(_))

  def subAt(cell: html.Element, xy: PosXY): Option[html.Element] =
    cellContentsIterator(cell, false)
      .find(_._2.exists(_ ==* xy))
      .map(_._1)

}
