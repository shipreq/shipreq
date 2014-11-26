package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
import shipreq.webapp.base.validation2._
import scalaz.effect.IO
import scalaz.syntax.bind._
import shipreq.webapp.client.util.ui.Util._

object Editors {

  type RU = ReactST[IO, Unit, Unit]
  val RU  = ReactS.FixT[IO, Unit]
  val nop = RU.ret(())

  def textEditor(node: Tag): Editor[String, String, RU, IO[Unit], Modifier] =
    Editor(ei => {
      val base = node(cls := ei.cssClass, value := ei.data)
      ei.editable match {
        case None =>
          base(readonly := true)
        case Some(cb) =>
          base(
            onchange  ~~> textChangeRecv(t => cb.t(OnChange(t), nop)),
            onblur    ~~> textChangeRecv(t => cb.t(OnEditFinished(t), nop)),
            onkeydown ~~> cancelOnEscape(cb.t(OnCancel, _)))
      }
    })

  def cancelOnEscape[X](f: RU => X): ReactKeyboardEventH => X =
    e => f(e.key match {
      case "Escape" => // TODO use KeyValue
        val t = e.target
        RU.callback[Unit](e.preventDefaultIO >> e.stopPropagationIO)(IO(t.blur()))
      case _ =>
        nop
    })

  val textInputEditor = textEditor(input)
  val textareaEditor  = textEditor(textarea)

  def renderWithError[A, B, C, D](editor: Editor[A, B, C, D, Modifier])(err: String): Editor[A, B, C, D, Modifier] =
    Editor(ei => div(editor render ei, div(cls := "errorMsg", err)))

  // -------------------------------------------------------------------------------------------------------------------

  def resolveEditorWithError[E, A, B, C, D, V](f: A => E, e: E => Editor[A, B, C, D, V]): Editor[A, B, C, D, V] =
    Editor(i => e(f(i.data)) render i)

  implicit final class EditorExt[A,B,C,D,V](val e: Editor[A,B,C,D,V]) extends AnyVal {
    type Self = Editor[A,B,C,D,V]

    def applyLiveCorrection(v: ValidatorS[_, B, _, _]): Self =
      e.pmodB { case OnChange(b) => v.liveCorrect(b) }

    def applyPostCorrection[U](v: CorrectionPart[B, U]): Self =
      e.pmodB { case OnEditFinished(b) => v.ci(v.correct_(b).value) }

    def applyPostCorrectionS[S, U](v: CorrectionPartS[S, B, U])(f: A => S): Self =
      e.pmodBx(a => _ => { case OnEditFinished(b) => v.ci(v.correct(f(a), b).value) })
  }

  implicit final class EditorExtV[A,B,C,D](val e: Editor[A,B,C,D,Modifier]) extends AnyVal {
    type Self = Editor[A,B,C,D,Modifier]

    def fromOptionalError: Option[String] => Self =
      _.fold(e)(renderWithError(e))

    def renderOptionalError(f: A => Option[String]): Self =
      Editor(i => resolveEditorWithError(f, fromOptionalError) render i)

    def applyInputValidation(v: Validator[A, _, _]): Self =
      renderOptionalError(i => v.correctAndValidate_(i).swap.toOption.map(_.toText))

    def applyInputValidationS[S, I](v: ValidatorS[S, I, _, _])(s: A => S, i: A => I): Self =
      renderOptionalError(a => v.correctAndValidate(s(a), i(a)).swap.toOption.map(_.toText))

    def applyInputValidationSL[S, I](v: ValidatorS[S, A, _, _]) =
      e.strengthL[S].applyInputValidationS(v)(_._1, _._2)
  }

}