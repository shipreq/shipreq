package shipreq.webapp.member.jsfacade

import org.scalajs.dom.html.{Input, TextArea}
import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.|

@JSGlobal("TFE")
@js.native
@nowarn
object TextFieldEdit extends js.Object {

  /** const field = document.querySelector('input[type="text"]');
    * textFieldEdit.insert(field, '🥳');
    * // Changes field's value from 'Party|' to 'Party🥳|' (where | is the cursor)
    */
  def insert(field: Input | TextArea, text: String): Unit = js.native

  /** Replaces the entire content, equivalent to field.value = 'New text!' but with undo support and by firing the
    * input event.
    */
  def set(field: Input | TextArea, text: String): Unit = js.native

  def wrapSelection(field          : Input | TextArea,
                    wrappingText   : String,
                    endWrappingText: String = js.native): Unit = js.native
}
