package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.scalajs.react._, vdom.html_<^._, ScalazReact._
import org.scalajs.dom.ext.KeyValue
import shipreq.webapp.base.data.On
import shipreq.webapp.base.lib.ClientUtil.textChangeRecv
import shipreq.webapp.client.project.widgets.Widgets.checkbox
import SimpleEditor._

object Editors {

  def textEditor(node: VdomTag): SimpleEditor[String] =
    Editor(ei => {
      val base = node(^.cls := ei.cssClass, ^.value := ei.data)
      ei.editable match {
        case None =>
          base(^.readOnly := true)
        case Some(cb) =>
          @inline def cbh(event: CallbackEvent[String], st: ST = nopST) = cb(callbackH(event, st))
          base(
            ^.onChange  ==> textChangeRecv(i => cbh(OnChange(i))),
            ^.onBlur    ==> textChangeRecv(i => cbh(OnEditFinished(i))),
            ^.onKeyDown ==> cancelOnEscape(s => cbh(OnCancel, s))) // esc doesn't trigger onKeyPress
      }
    })

  def cancelOnEscape(f: ST => Callback): ReactKeyboardEventFromHtml => Callback =
    e => e.key match {
      case KeyValue.Escape =>
        val t = e.target
        val st = ST.callback(e.preventDefaultCB >> e.stopPropagationCB, Callback(t.blur()))
        f(st)
      case _ =>
        Callback.empty
    }

  val textInputEditor = textEditor(<.input)
  val textareaEditor  = textEditor(<.textarea)

  // -------------------------------------------------------------------------------------------------------------------

  val checkboxEditor: SimpleEditor[On] =
    Editor(ei => {
      val base = checkbox(ei.data)(^.cls := ei.cssClass)
      ei.editable match {
        case None =>
          base(^.readOnly := true, ^.disabled := true)
        case Some(cb) =>
          @inline def cbh(event: CallbackEvent[On], st: ST = nopST) = cb(callbackH(event, st))
          def handleChange: ReactEventFromInput => Callback = e => {
            val b = On when e.target.checked
            cbh(OnChange(b)) >> cbh(OnEditFinished(b))
          }
          base(^.onChange ==> handleChange)
      }
    })

  val staticCheckbox: On => VdomElement =
    On.memo(on =>
      checkboxEditor render EditorI(on, "", None))

  // -------------------------------------------------------------------------------------------------------------------

  def renderWithError[A,B,M[_],S,C,D](editor: Editor[A,B,M,S,C,D,VdomElement], err: Option[String]): Editor[A,B,M,S,C,D,VdomElement] =
    Editor(i => <.div(
      editor render i,
      err.whenDefined(e => <.div(^.cls := "errorMsg", e))))
}
