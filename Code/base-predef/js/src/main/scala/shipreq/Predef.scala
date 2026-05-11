package shipreq

import scala.scalajs.js

// JS
object Predef extends PredefShared {

  @inline def console = org.scalajs.dom.console

  @inline def JSON = js.JSON

  @inline def BREAKPOINT() = js.special.debugger()

  def setStackTraceLimit(lines: Int): Unit =
    js.constructorOf[js.Error].stackTraceLimit = lines
}
