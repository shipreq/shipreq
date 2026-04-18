package shipreq.webapp.member.jsfacade

import org.scalajs.dom.html.{Input, TextArea}
import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.|

trait TextFieldEdit extends js.Object {
  import TextFieldEdit.Field

  // /** const field = document.querySelector('input[type="text"]');
  //   * textFieldEdit.insert(field, '🥳');
  //   * // Changes field's value from 'Party|' to 'Party🥳|' (where | is the cursor)
  //   */
  // def insert(field: Input | TextArea, text: String): Unit = js.native

  /** Replaces the entire content, equivalent to field.value = 'New text!' but with undo support and by firing the
    * input event.
    */
  def set(field: Field, text: String): Unit

  def wrapSelection(field: Field, wrappingText: String): Unit
  def wrapSelection(field: Field, wrappingText: String, endWrappingText: String): Unit
}

object TextFieldEdit {

  type Field = Input | TextArea

  @JSGlobal("TFE")
  @js.native
  object Real extends TextFieldEdit {
    override def set(field: Field, text: String): Unit = js.native
    override def wrapSelection(field: Field, wrappingText: String): Unit = js.native
    override def wrapSelection(field: Field, wrappingText: String, endWrappingText: String): Unit = js.native
  }

  // This is here so that tests can replace it.
  // React 17 + JSDOM + execCommand("insertText") doesn't work, where as it's fine in a real browser.
  var instance: TextFieldEdit = Real

  @inline def set(field: Field, text: String): Unit =
    instance.set(field, text)

  @inline def wrapSelection(field: Field, wrappingText: String): Unit =
    instance.wrapSelection(field, wrappingText)

  @inline def wrapSelection(field: Field, wrappingText: String, endWrappingText: String): Unit =
    instance.wrapSelection(field, wrappingText, endWrappingText)
}
