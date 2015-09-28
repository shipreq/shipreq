package shipreq.webapp.client.util

import japgolly.scalajs.react._
import scala.annotation.tailrec
import org.scalajs.dom._
import DomPatches._

object DomUtil {

  @inline implicit class PatchNode(private val n: Node) extends AnyVal {
    @inline def castDom[T <: Node] = n.asInstanceOf[T]
    @inline def castHtml = castDom[html.Element]
  }

  def checkModKeys(e       : ReactKeyboardEventH,
                   altKey  : Boolean = false,
                   ctrlKey : Boolean = false,
                   metaKey : Boolean = false,
                   shiftKey: Boolean = false): Boolean =
    e.altKey   == altKey   &&
    e.ctrlKey  == ctrlKey  &&
    e.metaKey  == metaKey  &&
    e.shiftKey == shiftKey

  def keyCodeSwitch(e       : ReactKeyboardEventH,
                    altKey  : Boolean = false,
                    ctrlKey : Boolean = false,
                    metaKey : Boolean = false,
                    shiftKey: Boolean = false)
                   (keyCodeSwitch: PartialFunction[Int, Callback]): CallbackOption[Unit] =
    for {
      _  <- CallbackOption.require(checkModKeys(e, altKey, ctrlKey, metaKey, shiftKey))
      _  <- CallbackOption.unless(e.defaultPrevented)
      cb <- CallbackOption.matchPF(e.nativeEvent.keyCode)(keyCodeSwitch)
      _  <- cb
      _  <- e.preventDefaultCB
    } yield ()

  /**
   * Determine the index of an element amongst its parent's children.
   *
   * @return ≥ 0
   */
  def siblingIndex(e: html.Element): CallbackTo[Int] =
    CallbackTo {
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

  def siblingAtOffset(e: html.Element, offset: Int): CallbackTo[Element] =
    siblingIndex(e).map { cur =>
      val sibs = e.parentElement.children
      val max = sibs.length
      var i = (cur + offset) % max
      if (i < 0)
        i += max
      sibs(i)
    }

  def isDragWithinNode(e: ReactDragEvent, node: Node): Boolean = {
    @inline def between(value: Double, from: Double, to: Double) =
      value >= from && value <= to
    val r = node.castHtml.getBoundingClientRect()
    between(e.clientX, r.left, r.right) && between(e.clientY, r.top, r.bottom)
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

}
