package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
import scalaz.{Applicative, Bind}
import scalaz.effect.IO
import scalaz.syntax.bind._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.validation._
import shipreq.base.util.Debug._
import UI._

object Editors {

  val ST  = ReactS.FixT[IO, Unit]
  type ST = ST.T[Unit]
  val nopST = ST.nop

  type SimpleEditor[I] = Editor[I, I, IO, Unit, Unit, IO[Unit], Tag]

  @inline private def callbackH[I](event: CallbackEvent[I], st: ST = nopST): CallbackH[I, IO, Unit, Unit] =
    CallbackH(event, st, ())

  def textEditor(node: Tag): SimpleEditor[String] =
    Editor(ei => {
      val base = node(cls := ei.cssClass, value := ei.data)
      ei.editable match {
        case None =>
          base(readonly := true)
        case Some(cb) =>
          @inline def cbh(event: CallbackEvent[String], st: ST = nopST) = cb(callbackH(event, st))
          base(
            onchange  ~~> textChangeRecv(i => cbh(OnChange(i))),
            onblur    ~~> textChangeRecv(i => cbh(OnEditFinished(i))),
            onkeydown ~~> cancelOnEscape(s => cbh(OnCancel, s))) // esc doesn't trigger onkeypress
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

  val textInputEditor = textEditor(input)
  val textareaEditor  = textEditor(textarea)

  val checkboxEditor: SimpleEditor[Boolean] =
    Editor(ei => {
      val base = checkbox(ei.data)(cls := ei.cssClass)
      ei.editable match {
        case None =>
          base(readonly := true)
        case Some(cb) =>
          @inline def cbh(event: CallbackEvent[Boolean], st: ST = nopST) = cb(callbackH(event, st))
          def handleChange: ReactEventI => IO[Unit] = e => {
            val b = e.target.checked
            cbh(OnChange(b)) >> cbh(OnEditFinished(b))
          }
          base(onchange ~~> handleChange)
      }
    })

  def renderWithError[A,B,M[_],S,C,D](editor: Editor[A,B,M,S,C,D,Tag], err: Option[String]): Editor[A,B,M,S,C,D,Tag] =
    Editor(ei => div(
      editor render ei,
      err.map(e => div(cls := "errorMsg", e))))

  // -------------------------------------------------------------------------------------------------------------------

  // TODO Move to implicits object, or add implicit defs to editor itself
  implicit final class EditorExt[A,B,M[_],S,C,D,V](val e: Editor[A,B,M,S,C,D,V]) extends AnyVal {

    def applyLiveCorrection(v: Validator[_, B, _, _]): Editor[A,B,M,S,C,D,V] =
      e.pmodB { case OnChange(b) => v.liveCorrect(b) }

    def applyPostCorrectionU[U](v: CorrectionPartU[B, U]): Editor[A,B,M,S,C,D,V] =
      e.pmodB { case OnEditFinished(b) => v.ci(v.correctU(b).value) }

    def applyPostCorrection[T, U](v: CorrectionPart[T, B, U])(f: A => T): Editor[A,B,M,S,C,D,V] =
      e.modCallbacksA(a =>
        _.pmodB{ case OnEditFinished(b) => v.ci(v.correct(f(a), b).value) })

    def applyOnEditFinishedK[K](f: K => ReactST[M, S, Unit])(g: A => K)(implicit M: Bind[M]): Editor[A,B,M,S,C,D,V] =
      e.paddSTA(a => {
        case OnEditFinished(_) => f(g(a))
      })
  }

  implicit final class EditorExtV[A,B,M[_],S,C,D](val e: Editor[A,B,M,S,C,D,Tag]) extends AnyVal {

    def renderOptionalError(f: A => Option[String]): Editor[A,B,M,S,C,D,Tag] =
      Editor(i => renderWithError(e, f(i.data)) render i)

    def applyInputValidationU(v: ValidatorU[A, _, _]): Editor[A,B,M,S,C,D,Tag] =
      renderOptionalError(i => v.correctAndValidateU(i).swap.toOption.map(_.toText))

    def applyInputValidation[T, I](v: Validator[T, I, _, _])(s: A => T, i: A => I): Editor[A,B,M,S,C,D,Tag] =
      renderOptionalError(a => v.correctAndValidate(s(a), i(a)).swap.toOption.map(_.toText))

    def applyInputValidationL[T, I](v: Validator[T, A, _, _]) =
      e.strengthL[T].applyInputValidation(v)(_._1, _._2)
  }

  implicit final class EditorExtII[I,M[_],S,C,D](val e: Editor[I,I,M,S,C,D,Tag]) extends AnyVal {

    def applyValidatorU(v: ValidatorU[I, _, _]): Editor[I,I,M,S,C,D,Tag] =
      e.applyInputValidationU(v)
        .applyLiveCorrection(v)
        .applyPostCorrectionU(v.cp)

    def applyValidator[T](v: Validator[T, I, _, _]): Editor[(T, I), I, M, S, C, D, Tag] =
      e.applyInputValidationL(v)
        .applyLiveCorrection(v)
        .applyPostCorrection(v.cp)(_._1)
  }

  def applyRowUpdateAndRevert[A, FV, M[_] : Bind : Applicative, S, F, D, V, K, P, I](
      e         : Editor[A, FV, M, S, F, D, V],
      savedStore: SavedRowStore[S, K, P, I],
      newStore  : NewRowStore[S, I])
      (k        : A => Option[K])
      (implicit wf: F <:< FieldSet[P, I]#Field, wv: FV <:< FieldSet[P, I]#FieldValue)
      : Editor[A, FV, M, S, F, D, V] =
    e.modCallbacksA(a =>
      k(a) match {
        case None =>
          _.paddST {
            case OnChange(v)       => newStore.setFieldST(v)
            case OnEditFinished(v) => newStore.setFieldST(v)
          }
        case Some(id) =>
          h => h.paddST {
            case OnChange(v)       => savedStore.setFieldST(id, v)
            case OnEditFinished(v) => savedStore.setFieldST(id, v)
            case OnCancel          => savedStore.revertFieldST(id, h.data)
          }
      }
    )
}