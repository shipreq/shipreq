package shipreq.webapp.client.jsfacade

import japgolly.scalajs.react._
import scalajs.js.{undefined, Dictionary, Dynamic, Object, UndefOr}
import shipreq.base.util.Memo
import ReactCollapse._

/**
 * Component-wrapper for collapse animation with react-motion for elements with variable (and dynamic) height.
 *
 * https://github.com/nkbt/react-collapse
 */
object ReactCollapse {
  type P = Object
  type S = Nothing

  val Factory: JsComponentC[P, S, TopNode] = {
    val ReactCollapse = Dynamic.global.ReactCollapse.asInstanceOf[JsComponentType[P, S, TopNode]]
    React.createFactory[P, S, TopNode](ReactCollapse)
  }

  @inline def apply(isOpened: Boolean): ReactCollapse =
    applyFn(isOpened)

  val applyFn: Boolean => ReactCollapse =
    Memo.bool(b =>
      new ReactCollapse(isOpened = b))
}

class ReactCollapse(isOpened    : Boolean,
                    fixedHeight : UndefOr[Double]        = undefined,
                    springConfig: UndefOr[Array[Double]] = undefined) {

  val toJs: P = {
    val o = Dictionary.empty[Any]
    o("isOpened") = isOpened
    fixedHeight  foreach (o("fixedHeight")  = _)
    springConfig foreach (o("springConfig") = _)
    o.asInstanceOf[P]
  }

  def apply(children: ReactNode*): ReactComponentU_ =
    Factory(toJs, children: _*)
}
