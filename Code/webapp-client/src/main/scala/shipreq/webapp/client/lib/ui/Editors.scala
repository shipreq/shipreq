package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom.ext.KeyValue
import scalaz.effect.IO
import scalaz.syntax.bind.ToBindOps
import shipreq.webapp.client.util.On
import UI._
import SimpleEditor._

object Editors {

  def textEditor(node: ReactTag): SimpleEditor[String] =
    Editor(ei => {
      val base = node(^.cls := ei.cssClass, ^.value := ei.data)
      ei.editable match {
        case None =>
          base(^.readOnly := true)
        case Some(cb) =>
          @inline def cbh(event: CallbackEvent[String], st: ST = nopST) = cb(callbackH(event, st))
          base(
            ^.onChange  ~~> textChangeRecv(i => cbh(OnChange(i))),
            ^.onBlur    ~~> textChangeRecv(i => cbh(OnEditFinished(i))),
            ^.onKeyDown ~~> cancelOnEscape(s => cbh(OnCancel, s))) // esc doesn't trigger onKeyPress
      }
    })

  def cancelOnEscape(f: ST => IO[Unit]): ReactKeyboardEventH => IO[Unit] =
    e => e.key match {
      case KeyValue.Escape =>
        val t = e.target
        val st = ST.callback(e.preventDefaultIO >> e.stopPropagationIO, IO(t.blur()))
        f(st)
      case _ =>
        IO(())
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
          def handleChange: ReactEventI => IO[Unit] = e => {
            val b = On <~ e.target.checked
            cbh(OnChange(b)) >> cbh(OnEditFinished(b))
          }
          base(^.onChange ~~> handleChange)
      }
    })

  val staticCheckbox: On => ReactElement =
    On.memo(on =>
      checkboxEditor render EditorI(on, "", None))

  // -------------------------------------------------------------------------------------------------------------------

  def renderWithError[A,B,M[_],S,C,D](editor: Editor[A,B,M,S,C,D,ReactElement], err: Option[String]): Editor[A,B,M,S,C,D,ReactElement] =
    Editor(i => <.div(
      editor render i,
      err.map(e => <.div(^.cls := "errorMsg", e))))
}
