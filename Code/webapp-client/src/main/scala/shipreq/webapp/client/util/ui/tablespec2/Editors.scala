package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
import scalaz.{Applicative, Bind}
import scalaz.effect.IO
import scalaz.syntax.bind._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.validation2._
import shipreq.webapp.client.util.ui.Util._

object Editors {

  type RU = ReactST[IO, Unit, Unit]
  val RU  = ReactS.FixT[IO, Unit]
  val nop = RU.nop

  type SimpleEditor[I] = Editor[I, I, RU, IO[Unit], Modifier]

  def textEditor(node: Tag): SimpleEditor[String] =
    Editor(ei => {
      val base = node(cls := ei.cssClass, value := ei.data)
      ei.editable match {
        case None =>
          base(readonly := true)
        case Some(cb) =>
          base(
            onchange  ~~> textChangeRecv(t => cb(OnChange(t), nop)),
            onblur    ~~> textChangeRecv(t => cb(OnEditFinished(t), nop)),
            onkeydown ~~> cancelOnEscape(cb(OnCancel, _)))
      }
    })

  def cancelOnEscape[X](f: RU => X): ReactKeyboardEventH => X =
    e => f(e.key match {
      case "Escape" => // TODO use KeyValue
        val t = e.target
        RU.callback[Unit](e.preventDefaultIO >> e.stopPropagationIO, IO(t.blur()))
      case _ =>
        nop
    })

  val textInputEditor = textEditor(input)
  val textareaEditor  = textEditor(textarea)

  val checkboxEditor: SimpleEditor[Boolean] =
    Editor(ei => {
      val base = checkbox(ei.data)(cls := ei.cssClass)
      ei.editable match {
        case None =>
          base(readonly := true)
        case Some(cb) =>
          def handleChange: ReactEventI => IO[Unit] = e => {
            val b = e.target.checked
            cb(OnChange(b), nop) >> cb(OnEditFinished(b), nop)
          }
          base(onchange ~~> handleChange)
      }
    })

  def renderWithError[A, B, C, D](editor: Editor[A, B, C, D, Modifier])(err: String): Editor[A, B, C, D, Modifier] =
    Editor(ei => div(editor render ei, div(cls := "errorMsg", err)))

  // -------------------------------------------------------------------------------------------------------------------

  def resolveEditorWithError[E, A, B, C, D, V](f: A => E, e: E => Editor[A, B, C, D, V]): Editor[A, B, C, D, V] =
    Editor(i => e(f(i.data)) render i)

  // TODO Move to implicits object, or add implicit defs to editor itself
  implicit final class EditorExt[A,B,C,D,V](val e: Editor[A,B,C,D,V]) extends AnyVal {
    type Self = Editor[A,B,C,D,V]

    def applyLiveCorrection(v: Validator[_, B, _, _]): Self =
      e.pmodB { case OnChange(b) => v.liveCorrect(b) }

    def applyPostCorrectionU[U](v: CorrectionPartU[B, U]): Self =
      e.pmodB { case OnEditFinished(b) => v.ci(v.correct_(b).value) }

    def applyPostCorrection[S, U](v: CorrectionPart[S, B, U])(f: A => S): Self =
      e.pmodBx(a => _ => { case OnEditFinished(b) => v.ci(v.correct(f(a), b).value) })
  }

  implicit final class EditorExtV[A,B,C,D](val e: Editor[A,B,C,D,Modifier]) extends AnyVal {
    type Self = Editor[A,B,C,D,Modifier]

    def fromOptionalError: Option[String] => Self =
      _.fold(e)(renderWithError(e))

    def renderOptionalError(f: A => Option[String]): Self =
      Editor(i => resolveEditorWithError(f, fromOptionalError) render i)

    def applyInputValidationU(v: ValidatorU[A, _, _]): Self =
      renderOptionalError(i => v.correctAndValidate_(i).swap.toOption.map(_.toText))

    def applyInputValidation[S, I](v: Validator[S, I, _, _])(s: A => S, i: A => I): Self =
      renderOptionalError(a => v.correctAndValidate(s(a), i(a)).swap.toOption.map(_.toText))

    def applyInputValidationL[S, I](v: Validator[S, A, _, _]) =
      e.strengthL[S].applyInputValidation(v)(_._1, _._2)
  }

  implicit final class EditorExtII[I,C,D,V](val e: Editor[I,I,C,D,Modifier]) extends AnyVal {

    def applyValidatorU(v: ValidatorU[I, _, _]): Editor[I, I, C, D, Modifier] =
      e.applyInputValidationU(v)
        .applyLiveCorrection(v)
        .applyPostCorrectionU(v.cp)

    def applyValidator[S](v: Validator[S, I, _, _]): Editor[(S, I), I, C, D, Modifier] =
      e.applyInputValidationL(v)
        .applyLiveCorrection(v)
        .applyPostCorrection(v.cp)(_._1)
  }

  implicit final class EditorExtIII[M[_], S,A,B,C1,D,V](val e: Editor[A,B,(C1,ReactST[M, S, Unit]),D,V]) extends AnyVal {
    type Self = Editor[A, B, (C1,ReactST[M, S, Unit]), D, V]
    def applyOnEditFinished[K](f: K => ReactST[M, S, Unit])(g: A => K)(implicit M: Bind[M]): Self =
      e.modCallbacksA(a =>
        _.pmodC(c => {
          case OnEditFinished(_) => c map2 (_ >> f(g(a)))
        })
      )
  }

  def applyRowUpdateAndRevert[M[_] : Bind : Applicative, S, K, P, I, A, D, V, F, FV](
      e         : Editor[A, FV, (F, ReactST[M, S, Unit]), D, V],
      savedStore: SavedRowStore[S,K,P,I],
      newStore  : NewRowStore[S,I])
      (k        : A => Option[K])
      (implicit wf: F <:< FieldSet[P, I]#Field, wv: FV <:< FieldSet[P, I]#FieldValue)
      : Editor[A, FV, (F, ReactST[M, S, Unit]), D, V] =
    e.modCallbacksA(a =>
      k(a) match {
        case None =>
          _.pmodC(c => {
            case OnChange(v) => c map2 (_ >> newStore.setFieldST(v))
          })
        case Some(id) =>
          _.pmodC(c => {
            case OnChange(v) => c map2 (_ >> savedStore.setFieldST(id, v))
            case OnCancel    => c map2 (_ >> savedStore.revertFieldST(id, c._1))
          })
      })

}