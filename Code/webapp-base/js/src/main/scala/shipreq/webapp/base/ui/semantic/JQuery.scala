package shipreq.webapp.base.ui.semantic

import org.scalajs.dom.Node
import scala.scalajs.js
import scala.scalajs.js.|

/** Wrapper for JQuery with Semantic UI extensions.
  */
@js.native
@nowarn
trait JQuery extends js.Object {

  def is(sel: String): Boolean = js.native
  def find(sel: String): JQuery = js.native
  def toggleClass(cls: String): JQuery = js.native

  def dimmer(options: js.Object = js.native): JQuery = js.native

  def dropdown(options: String | Dropdown.JsOptions = js.native, arg: String = js.native): JQuery = js.native

  def modal(): JQuery = js.native
  def modal(command: String): JQuery = js.native
  def modal(command: js.Object): JQuery = js.native

  def accordion(command: String = js.native): JQuery = js.native

  def popup(options: Popup.Js.Options = js.native): JQuery = js.native

  def transition(obj: js.Object): JQuery = js.native
  def transition(animation: Transition): JQuery = js.native
  def transition(animation: Transition, duration: String): JQuery = js.native
}

object JQuery {
  def byId(id: String): JQuery =
    js.Dynamic.global.$("#" + id).asInstanceOf[JQuery]

  def apply(n: Node): JQuery =
    js.Dynamic.global.$(n).asInstanceOf[JQuery]
}