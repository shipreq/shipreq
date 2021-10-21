package shipreq.webapp.member.jsfacade

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.|

/** Automatically adjusts textarea height to fit text.
  *
  * https://github.com/jackmoore/autosize
  */
@JSGlobal("autosize")
@js.native
@nowarn
object Autosize extends js.Object {

  type Target  = dom.Element
  type Targets = Target | dom.NodeList[dom.Node]

  def apply(targets: Targets): Unit = js.native

  def update(targets: Targets): Unit = js.native

  def destroy(targets: Targets): Unit = js.native

  /*
  private def postEvent(target: Target, name: String): Unit = {
    val evt = dom.document.createEvent("Event")
    evt.initEvent("autosize:" + name, true, false)
    target.dispatchEvent(evt)
  }

  def update(target: Target): Unit =
    postEvent(target, "update")

  def destroy(target: Target): Unit =
    postEvent(target, "destroy")
  */
}
