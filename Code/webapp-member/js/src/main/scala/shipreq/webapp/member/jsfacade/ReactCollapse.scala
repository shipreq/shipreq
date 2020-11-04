package shipreq.webapp.member.jsfacade

import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

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
    var isOpened: Boolean
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
