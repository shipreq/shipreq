package shipreq.webapp.client.base.jsfacade

import org.scalajs.dom
import scala.scalajs.js, js.|

/** Automatically adjusts textarea height to fit text.
  *
  * https://github.com/jackmoore/autosize
  */
object Autosize {

  type Target  = dom.Element
  type Targets = Target | dom.NodeList

  def init(targets: Targets): Unit =
    js.Dynamic.global.autosize(targets.asInstanceOf[js.Any])

  def update(targets: Targets): Unit =
    js.Dynamic.global.autosize.update(targets.asInstanceOf[js.Any])

  def destroy(targets: Targets): Unit =
    js.Dynamic.global.autosize.destroy(targets.asInstanceOf[js.Any])

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
