package shipreq.webapp.base.jsfacade

import japgolly.scalajs.react._
import scalajs.js
import scalajs.js.annotation.JSGlobal
import shipreq.base.util.Memo

/**
 * Component-wrapper for collapse animation with react-motion for elements with variable (and dynamic) height.
 *
 * https://github.com/nkbt/react-collapse
 */
object ReactCollapse {

  @JSGlobal("ReactCollapse")
  @js.native
  object RawComp extends js.Object

  @js.native
  trait Props extends js.Object {
    var isOpened: Boolean = js.native
  }

  val component = JsComponent[Props, Children.Varargs, Null](RawComp)

  @inline def apply(isOpened: Boolean) =
    applyFn(isOpened)

  val applyFn =
    Memo.bool { b =>
      val p = (new js.Object).asInstanceOf[Props]
      p.isOpened = b
      component.mapCtorType(_ withProps p)
    }
}
