package shipreq.webapp.member.jsfacade

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@JSGlobal("screenfull")
@js.native
@nowarn
object Screenfull extends js.Object {

  val isEnabled: Boolean = js.native
  def request(): Unit    = js.native
  def exit   (): Unit    = js.native
  def toggle (): Unit    = js.native
}
