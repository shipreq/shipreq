package shipreq.webapp.base.feature.tablenav

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.ReactExt_DomNode
import japgolly.univeq._
import org.scalajs.dom.{ClientRect, html}
import scala.annotation.tailrec
import scalaz.{-\/, \/, \/-}
import shipreq.webapp.base.lib.DomUtil._

private[tablenav] object Logic {

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

  def rowIterator(table: html.Table): Iterator[html.TableRow] =
    table
      .children.iterator
      .filter(t => t.tagName ==* "TBODY" || t.tagName ==* "THEAD")
      .flatMap(_.domAsHtml
        .children.iterator
        .filter(_.tagName ==* "TR")
        .map(_.domCast[html.TableRow])
        .filter(rowContentsIterator(_).nonEmpty))

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
      .filterHtml
      .focusable
      .zipWithIndex
      .map { case (e, j) => e -> PosXY(j, 0) }

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
