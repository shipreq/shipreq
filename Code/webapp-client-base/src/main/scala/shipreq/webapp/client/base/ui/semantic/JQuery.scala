package shipreq.webapp.client.base.ui.semantic

import org.scalajs.dom.Node
import scala.scalajs.js

/** Wrapper for JQuery with Semantic UI extensions.
  */
@js.native
trait JQuery extends js.Object {

  def find(sel: String): JQuery = js.native

  def dimmer   (options: js.Object = js.native): Unit = js.native
  def dropdown (options: js.Object = js.native): Unit = js.native
  def modal    (command: String = js.native): Unit = js.native
  def accordion(command: String = js.native): Unit = js.native
}

object JQuery {
  def byId(id: String): JQuery =
    js.Dynamic.global.$("#" + id).asInstanceOf[JQuery]

  def apply(n: Node): JQuery =
    js.Dynamic.global.$(n).asInstanceOf[JQuery]
}