package shipreq.webapp.client.project

import japgolly.scalajs.react.test._
import scala.scalajs.js
import shipreq.webapp.member.jsfacade.TextFieldEdit

package object test {

  object PrepareEnv {
    def apply(): Unit = ()

    // Initialise styles
    shipreq.webapp.client.project.app.Style

    // React 17 + JSDOM + execCommand("insertText") doesn't work
    TextFieldEdit.instance = testTextFieldEdit

    // JSDOM doesn't support innerText
    js.eval(
      """if (typeof HTMLElement !== 'undefined' && !Object.prototype.hasOwnProperty.call(HTMLElement.prototype, 'innerText')) {
        |  Object.defineProperty(HTMLElement.prototype, 'innerText', {
        |    get: function() { return this.textContent; },
        |    set: function(v) { this.textContent = v; },
        |    configurable: true
        |  });
        |}
        |""".stripMargin)
  }

  private def testTextFieldEdit: TextFieldEdit = new TextFieldEdit {
    import TextFieldEdit.Field

    override def set(field: Field, text: String): Unit = {
      field.asInstanceOf[js.Dynamic].value = text
      SimEvent.Change(text).simulate(field)
    }

    override def wrapSelection(field: Field, wrap: String): Unit =
      wrapSelection(field, wrap, wrap)

    override def wrapSelection(field: Field, wrap: String, wrapEnd: String): Unit = {
      val f        = field.asInstanceOf[js.Dynamic]
      val text     = f.value.asInstanceOf[String]
      val selStart = f.selectionStart.asInstanceOf[Int]
      val selEnd   = f.selectionEnd.asInstanceOf[Int]
      val newText  = text.patch(selEnd, wrapEnd, 0).patch(selStart, wrap, 0)
      set(field, newText)

      // Restore the selection around the previously-selected text
      f.selectionStart = wrap.length + selStart
      f.selectionEnd   = wrap.length + selEnd
    }
  }
}
