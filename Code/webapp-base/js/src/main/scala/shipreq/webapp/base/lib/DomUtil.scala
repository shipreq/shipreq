package shipreq.webapp.base.lib

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.{raw => _, _}
import scala.annotation.tailrec
import scalajs.js
import org.scalajs.dom._
import shipreq.base.util.Util

object DomUtil {

  val SvgNS = "http://www.w3.org/2000/svg"

  @tailrec
  private def findParent(e: html.Element, f: html.Element => Boolean, self: Boolean): Option[html.Element] = {
    if (self && f(e))
      Some(e)
    else
      Option(e.parentElement) match {
        case Some(p) => findParent(p, f, self = true)
        case None    => None
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Extensions

//  @inline implicit class PatchNode(private val n: Node) extends AnyVal {
//  }

  @inline implicit class PatchHtmlElement(private val e: html.Element) extends AnyVal {
    def _disabled: js.UndefOr[Boolean] =
      e.asInstanceOf[js.Dynamic].disabled.asInstanceOf[js.UndefOr[Boolean]]

    def disabledSafe: Boolean =
      _disabled.getOrElse(false)

    def findParent(f: html.Element => Boolean, self: Boolean = false): Option[html.Element] =
      DomUtil.findParent(e, f, self = self)
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

    def indices: scala.Range =
      0 until c.length

    def iterator: Iterator[Element] =
      indices.iterator.map(c.apply)

    def deepIteratorDepthFirst: Iterator[Element] =
      iterator.flatMap(e => Iterator.single(e) ++ e.children.deepIteratorDepthFirst)

    def deepIteratorBreadthFirst: Iterator[Element] =
      iterator ++ iterator.flatMap(_.children.deepIteratorBreadthFirst)
  }

  @inline implicit class NodeIteratorExt[N >: html.Element <: Node](private val it: Iterator[N]) extends AnyVal {
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

  def activeHtmlElement: CallbackTo[Option[html.Element]] =
    CallbackTo(
      Option(document.activeElement)
        .flatMap(_.domToHtml)
        .filterNot(_ eq document.body))

  def focusableChildren(e: Element): Iterator[html.Element] =
    e.children.deepIteratorDepthFirst.focusable

  def isFocusable(e: html.Element): Boolean = {
    @inline def hasTabIndex =
      e.tabIndex >= 0 || e.hasAttribute("tabIndex") // .tabIndex == -1 when unspecified so check if specified

    @inline def enabled =
      e._disabled.forall(!_)

    @inline def blacklisted = e match {
      // Chrome (at least) doesn't allow anchors without hrefs to have focus
      case a: html.Anchor => a.href.isEmpty
      case _ => false
    }

    hasTabIndex && enabled && !blacklisted
  }

  def isDragWithinNode(e: ReactDragEvent, node: Node): Boolean = {
    @inline def between(value: Double, from: Double, to: Double) =
      value >= from && value <= to
    val r = node.domAsHtml.getBoundingClientRect()
    between(e.clientX, r.left, r.right) && between(e.clientY, r.top, r.bottom)
  }

  /**
   * When a Button in the cell is clicked, we still get the event here in which case, the focus is set after the
   * button callback runs, meaning that (because separate modState()s don't compose) we trample the state change made by
   * the button, and replace it with a focus update.
   *
   * Rather than force all cell children to stop propagation of events, we apply so logic here to filter the events to
   * which we react.
   */
  def doesEventTargetCell(e: ReactEventFromHtml): Boolean =
    e.target == e.currentTarget ||
      (try e.target.tabIndex < 0 catch { case _: Throwable => false }) // .tabIndex is undefined from tests

  def asEventDefaultWhenTargetsCell(e: ReactEventFromHtml)(handler: CallbackOption[Unit]): CallbackOption[Unit] =
    (CallbackOption.require(doesEventTargetCell(e)) >> handler).asEventDefault(e)

  def focusParentOnChildClose(parent: html.Element): Callback =
    for (focused <- activeHtmlElement) yield
      // If this cell's child is focused, or there is no focus at all, then focus this cell.
      // Otherwise, don't steal another element's focus
      if (focused.forall(parent.contains))
        parent.focus()

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
}
