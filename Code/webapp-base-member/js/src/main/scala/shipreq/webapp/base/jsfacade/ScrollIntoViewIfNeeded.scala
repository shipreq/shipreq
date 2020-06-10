package shipreq.webapp.base.jsfacade

import japgolly.scalajs.react.Callback
import org.scalajs.dom.html
import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSGlobal("scrollIntoView")
@js.native
object ScrollIntoViewIfNeededFacade extends js.Object {

  def apply(node: html.Element, options: Options): Unit = js.native

  @js.native
  trait Options extends js.Object {
    var behavior                  : js.UndefOr[ScrollBehavior            ] = js.native
    var block                     : js.UndefOr[ScrollLogicalPosition     ] = js.native
    var inline                    : js.UndefOr[ScrollLogicalPosition     ] = js.native
    var scrollMode                : js.UndefOr[ScrollMode                ] = js.native
    var boundary                  : js.UndefOr[CustomScrollBoundary      ] = js.native
    var skipOverflowHiddenElements: js.UndefOr[SkipOverflowHiddenElements] = js.native
  }

  sealed trait ScrollBehavior        extends js.Any
  sealed trait ScrollLogicalPosition extends js.Any
  sealed trait ScrollMode            extends js.Any

  type CustomScrollBoundary = Any // Element | Element => boolean | null
  type SkipOverflowHiddenElements = Boolean
}

object ScrollIntoViewIfNeeded {
  import ScrollIntoViewIfNeededFacade._

  def apply(node: html.Element): Callback =
    apply(node, defaultOptions)

  def apply(node: html.Element, options: Options): Callback =
    Callback(ScrollIntoViewIfNeededFacade(node, options))

  def Options(): Options =
    (new js.Object).asInstanceOf[Options]

  @inline def auto   = "auto"  .asInstanceOf[ScrollBehavior]
  @inline def smooth = "smooth".asInstanceOf[ScrollBehavior]

  @inline def start   = "start"  .asInstanceOf[ScrollLogicalPosition]
  @inline def center  = "center" .asInstanceOf[ScrollLogicalPosition]
  @inline def end     = "end"    .asInstanceOf[ScrollLogicalPosition]
  @inline def nearest = "nearest".asInstanceOf[ScrollLogicalPosition]

  @inline def always   = "always"   .asInstanceOf[ScrollMode]
  @inline def ifNeeded = "if-needed".asInstanceOf[ScrollMode]

  private val defaultOptions: Options = {
    val o        = Options()
    o.behavior   = smooth
    o.scrollMode = ifNeeded
    o.block      = nearest
    o.inline     = nearest
    o
  }
}