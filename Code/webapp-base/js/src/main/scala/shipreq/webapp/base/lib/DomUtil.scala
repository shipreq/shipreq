package shipreq.webapp.base.lib

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.{raw => _, _}
import scala.annotation.tailrec
import scalajs.js
import org.scalajs.dom._
import shipreq.base.util.Util

object DomUtil {

  val SvgNS = "http://www.w3.org/2000/svg"

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Extensions

//  @inline implicit class PatchNode(private val n: Node) extends AnyVal {
//  }

  @inline implicit class PatchHtmlElement(private val e: html.Element) extends AnyVal {
    def _disabled: js.UndefOr[Boolean] =
      e.asInstanceOf[js.Dynamic].disabled.asInstanceOf[js.UndefOr[Boolean]]
  }

  @inline implicit class NodeListExt(private val n: NodeList) extends AnyVal {
    def iterator: Iterator[Node] =
      (0 until n.length).iterator.map(n.apply)
  }

  @inline implicit class DOMStringListExt(private val d: DOMStringList) extends AnyVal {
    def exists(f: String => Boolean): Boolean = {
      @tailrec def go(i: Int): Boolean =
        if (i == -1)
          false
        else if (f(d(i)))
          true
        else
          go(i - 1)
      go(d.length - 1)
    }
  }

  @inline implicit class PatchHTMLCollection(private val c: raw.HTMLCollection) extends AnyVal {
    def nonEmpty: Boolean =
      c.length > 0

    def isEmpty: Boolean =
      !nonEmpty

    def headOption: Option[Element] =
      if (nonEmpty) Some(head) else None

    def lastOption: Option[Element] =
      if (nonEmpty) Some(last) else None

    def head: Element =
      c(0)

    def last: Element =
      c(c.length - 1)

    def iterator: Iterator[Element] =
      (0 until c.length).iterator.map(c.apply)

    def deepIteratorDepthFirst: Iterator[Element] =
      iterator.flatMap(e => Iterator.single(e) ++ e.children.deepIteratorDepthFirst)

    def deepIteratorBreadthFirst: Iterator[Element] =
      iterator ++ iterator.flatMap(_.children.deepIteratorBreadthFirst)
  }

  @inline implicit class NodeIteratorExt[N >: html.Element <: Node](private val it: Iterator[N]) extends AnyVal {
    def asHtml: Iterator[html.Element] =
      it.asInstanceOf[Iterator[html.Element]]
    def filterHtml: Iterator[html.Element] =
      it.filterSubType[html.Element]
    def focusable: Iterator[html.Element] =
      filterHtml.focusable
  }

  @inline implicit class IteratorHtmlElementExt(private val it: Iterator[html.Element]) extends AnyVal {
    def focusable: Iterator[html.Element] =
      it.filter(isFocusable)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Functions

  def focusedHtmlElement: CallbackTo[Option[html.Element]] =
    CallbackTo(
      document.activeElement
        .domToHtml
        .filterNot(_ eq document.body))

  def focusableChildren(e: Element): Iterator[html.Element] =
    e.children.deepIteratorDepthFirst.focusable

  def isFocusable(e: html.Element): Boolean =
    (e.tabIndex >= 0
      || e.hasAttribute("tabIndex") // .tabIndex == -1 when unspecified too. Thus check if specified.
      ) && !e._disabled.exists(identity) // ignore disabled


  def isDragWithinNode(e: ReactDragEvent, node: Node): Boolean = {
    @inline def between(value: Double, from: Double, to: Double) =
      value >= from && value <= to
    val r = node.domAsHtml.getBoundingClientRect()
    between(e.clientX, r.left, r.right) && between(e.clientY, r.top, r.bottom)
  }

  def keyCodeSwitch(e       : ReactKeyboardEvent,
                    altKey  : Boolean = false,
                    ctrlKey : Boolean = false,
                    metaKey : Boolean = false,
                    shiftKey: Boolean = false)
                   (keyCodeSwitch: PartialFunction[Int, Callback]): CallbackOption[Unit] =
    CallbackOption.asEventDefault(e,
      CallbackOption.keyCodeSwitch(e, altKey, ctrlKey, metaKey, shiftKey)(keyCodeSwitch))

  /**
   * Determine the index of an element amongst its parent's children.
   *
   * @return ≥ 0
   */
  def siblingIndex(e: html.Element): Int = {
    var m: Option[Element] = Some(e)
    def next() = {
      m = Option(m.get.previousElementSibling)
      m.isDefined
    }
    var i = 0
    while (next())
      i += 1
    i
  }

  /**
   * @param f Given the index of `e`, return the index of the target element.
   */
  def siblingAt(e: html.Element, f: Int => Int): Element = {
    val cur = siblingIndex(e)
    val sibs = e.parentElement.children
    val i = Util.fitCollectionIndex(f(cur), sibs.length)
    sibs(i)
  }

  /**
   * @param index The target index. 0 = first, 1 = second, -1 = last, -2 = second last.
   */
  def siblingAtIndex(e: html.Element, index: Int) =
    siblingAt(e, _ => index)

  /**
   * @param offset The target index relative to the current index. 1 = next, -1 = prev
   */
  def siblingAtOffset(e: html.Element, offset: Int) =
    siblingAt(e, _ + offset)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Structures

  sealed abstract class Movement(val adjustIndex: Int => Int)
  object Movement {
    case object None extends Movement(identity)
    case object Prev extends Movement(_ - 1)
    case object Next extends Movement(_ + 1)
    case object Head extends Movement(_ => 0)
    case object Last extends Movement(_ => -1)
  }

  /**
   * Not very robust yet. Expects:
   *
   * - a THEAD with exactly 1 row.
   * - a TBODY with 0-n rows.
   * - at least one column.
   * - the same number of columns (no colspans) in each row.
   *
   * @param focus Either table>thead>tr>th or table>tbody>tr>td
   */
  final case class TableCellZipper(focus: html.Element) {
    @inline implicit private def autoCastHtml(e: Element) = e.domAsHtml

    def focusRow    : html.Element = focus.parentElement    // TH | TD
    def focusSection: html.Element = focusRow.parentElement // THEAD | TBODY

    val rowIndex: Int =
      focusSection.tagName match {
        case "THEAD" => 0
        case "TBODY" => siblingIndex(focusRow) + 1
      }

    val colIndex: Int =
      siblingIndex(focus)

    private def rowAtIndex(i: Int): html.Element = {
      val table = focusSection.parentElement
      assert("TABLE" == table.tagName, s"Expected TABLE, got: ${table.tagName}")
      def thead = table.children(0)
      val tbody = table.children(1)
      val j = Util.fitCollectionIndex(i, tbody.children.length + 1)
      if (j == 0)
        thead.children(0)
      else
        tbody.children(j - 1)
    }

    def move_-(m: Movement): TableCellZipper =
      if (m == Movement.None) this else
        TableCellZipper(siblingAt(focus, m.adjustIndex))

    def move_|(m: Movement): TableCellZipper =
      if (m == Movement.None) this else {
        val tgtRow  = rowAtIndex(m adjustIndex rowIndex)
        val tgtCell = tgtRow.children(colIndex)
        TableCellZipper(tgtCell)
      }
  }
}
