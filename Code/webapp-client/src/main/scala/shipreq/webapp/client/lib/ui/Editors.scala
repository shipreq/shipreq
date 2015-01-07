package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import scalaz.effect.IO
import scalaz.syntax.bind.ToBindOps
import shipreq.base.util.ScalaExt._
import UI._

object Editors {
  val ST    = ReactS.FixT[IO, Unit]
  type ST   = ST.T[Unit]
  val nopST = ST.nop

  type SimpleEditor[I] = Editor[I, I, IO, Unit, Unit, IO[Unit], ReactTag]

  def constSimpleEditor[I](v: ReactTag): SimpleEditor[I] =
    Editor(_ => v)

  @inline private def callbackH[I](event: CallbackEvent[I], st: ST = nopST): CallbackH[I, IO, Unit, Unit] =
    CallbackH(event, st, ())

  // -------------------------------------------------------------------------------------------------------------------

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
      case "Escape" => // TODO use KeyValue
        val t = e.target
        val st = ST.callback(e.preventDefaultIO >> e.stopPropagationIO, IO(t.blur()))
        f(st)
      case _ =>
        IO(())
    }

  val textInputEditor = textEditor(<.input)
  val textareaEditor  = textEditor(<.textarea)

  // -------------------------------------------------------------------------------------------------------------------

  val checkboxEditor: SimpleEditor[Boolean] =
    Editor(ei => {
      val base = checkbox(ei.data)(^.cls := ei.cssClass)
      ei.editable match {
        case None =>
          base(^.readOnly := true, ^.disabled := true)
        case Some(cb) =>
          @inline def cbh(event: CallbackEvent[Boolean], st: ST = nopST) = cb(callbackH(event, st))
          def handleChange: ReactEventI => IO[Unit] = e => {
            val b = e.target.checked
            cbh(OnChange(b)) >> cbh(OnEditFinished(b))
          }
          base(^.onChange ~~> handleChange)
      }
    })

  val List(staticCheckboxOn, staticCheckboxOff) = List(true, false).map(b =>
    checkboxEditor render EditorI(b, "", None))

  def staticCheckbox(checked: Boolean): ReactTag =
    if (checked) staticCheckboxOn else staticCheckboxOff

  // -------------------------------------------------------------------------------------------------------------------

  def renderWithError[A,B,M[_],S,C,D](editor: Editor[A,B,M,S,C,D,ReactTag], err: Option[String]): Editor[A,B,M,S,C,D,ReactTag] =
    Editor(i => <.div(
      editor render i,
      err.map(e => <.div(^.cls := "errorMsg", e))))
}
