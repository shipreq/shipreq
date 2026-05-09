package shipreq.webapp.base.util

import japgolly.scalajs.react._
import org.scalajs.dom.window

object Browser {

  val isMac: Boolean =
    window.navigator.platform.contains("Mac") ||
      window.navigator.platform.toLowerCase.contains("darwin") ||
      window.navigator.platform.toLowerCase.contains("os x") ||
      window.navigator.platform.toLowerCase.contains("apple")

  def cmdOrCtrlKeyCodeSwitch[A](e       : ReactKeyboardEvent,
                                altKey  : Boolean = false,
                                shiftKey: Boolean = false)
                               (switch  : PartialFunction[Int, CallbackTo[A]]): CallbackOption[A] =
    CallbackOption.keyCodeSwitch(
      e,
      altKey   = altKey,
      ctrlKey  = !isMac,
      metaKey  = isMac,
      shiftKey = shiftKey)(
      switch)
}
