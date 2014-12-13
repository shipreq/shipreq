package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import scalaz.{Applicative, Bind, ~>}
import scalaz.effect.IO
import shipreq.webapp.base.validation._

abstract class EditorExt {
  @inline implicit final def ___EditorExt_1    [A,B,M[_],S,C,D,V](e: Editor[A,B,M,S,C,D,V       ]): EditorExt.EditorExt_1    [A,B,M,S,C,D,V] = new EditorExt.EditorExt_1(e)
  @inline implicit final def ___EditorExt_Tag  [A,B,M[_],S,C,D  ](e: Editor[A,B,M,S,C,D,ReactTag]): EditorExt.EditorExt_Tag  [A,B,M,S,C,D  ] = new EditorExt.EditorExt_Tag(e)
  @inline implicit final def ___EditorExt_IITag[I  ,M[_],S,C,D  ](e: Editor[I,I,M,S,C,D,ReactTag]): EditorExt.EditorExt_IITag[I,  M,S,C,D  ] = new EditorExt.EditorExt_IITag(e)
}

object EditorExt extends EditorExt {

  final class EditorExt_1[A,B,M[_],S,C,D,V](val e: Editor[A,B,M,S,C,D,V]) extends AnyVal {

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

    def applyRowUpdate[K, P](savedStore: SavedRowStore[S, K, P, B])
                            (k: A => K)
                            (implicit A: Applicative[M], B: Bind[M]): Editor[A, B, M, S, C, D, V] =
      e.modCallbacksA(a =>
        h => h.paddST {
          case OnChange(v)       => savedStore.setIST(k(a), v)
          case OnEditFinished(v) => savedStore.setIST(k(a), v)
        })

    def applyRowUpdateAndRevert[K, P, I](savedStore: SavedRowStore[S, K, P, I], newStore: NewRowStore[S, I])
                                        (k: A => Option[K])
                                        (implicit A: Applicative[M], B: Bind[M],
                                         wv: B <:< FieldSet[P, I]#FieldValue,
                                         wf: C <:< FieldSet[P, I]#Field): Editor[A,B,M,S,C,D,V] =
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

    def editableByRowStatus(c: ComponentStateFocus[S])
                           (implicit ev: IO[Unit] =:= D, M: M ~> IO): RowStatus => Option[e.Editable] = {
      val canedit = e.editable(c runState _.st)
      rs => rs match {
        case RowStatus.Sync | RowStatus.Failed(_) => canedit
        case RowStatus.Locked                     => None
      }
    }

  }

  final class EditorExt_Tag[A,B,M[_],S,C,D](val e: Editor[A,B,M,S,C,D,ReactTag]) extends AnyVal {

    def renderOptionalError(f: A => Option[String]): Editor[A,B,M,S,C,D,ReactTag] =
      Editor(i => Editors.renderWithError(e, f(i.data)) render i)

    def wrapInLabel(f: (A, ReactTag) => TagMod): Editor[A,B,M,S,C,D,ReactTag] =
      Editor(i => <.label(f(i.data, e render i)))

    def labelSuffix(f: A => TagMod): Editor[A,B,M,S,C,D,ReactTag] =
      wrapInLabel((a, i) => Seq(i, f(a)))

    def applyInputValidationU(v: ValidatorU[A, _, _]): Editor[A,B,M,S,C,D,ReactTag] =
      renderOptionalError(i => v.correctAndValidateU(i).swap.toOption.map(_.toText))

    def applyInputValidation[T, I](v: Validator[T, I, _, _])(s: A => T, i: A => I): Editor[A,B,M,S,C,D,ReactTag] =
      renderOptionalError(a => v.correctAndValidate(s(a), i(a)).swap.toOption.map(_.toText))

    def applyInputValidationL[T, I](v: Validator[T, A, _, _]) =
      e.strengthL[T].applyInputValidation(v)(_._1, _._2)
  }

  final class EditorExt_IITag[I,M[_],S,C,D](val e: Editor[I,I,M,S,C,D,ReactTag]) extends AnyVal {

    def applyValidatorU(v: ValidatorU[I, _, _]): Editor[I,I,M,S,C,D,ReactTag] =
      e.applyInputValidationU(v)
        .applyLiveCorrection(v)
        .applyPostCorrectionU(v.cp)

    def applyValidator[T](v: Validator[T, I, _, _]): Editor[(T, I), I, M, S, C, D, ReactTag] =
      e.applyInputValidationL(v)
        .applyLiveCorrection(v)
        .applyPostCorrection(v.cp)(_._1)
  }
}
