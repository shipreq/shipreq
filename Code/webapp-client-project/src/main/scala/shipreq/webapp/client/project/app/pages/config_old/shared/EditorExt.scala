package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.scalajs.react._, vdom.html_<^._, ScalazReact._
import scalaz.{Monad, ~>}
import shipreq.webapp.base.validation._

abstract class EditorExt {
  @inline implicit final def ___EditorExt_1    [A,B,M[_],S,C,D,V](e: Editor[A,B,M,S,C,D,V          ]): EditorExt.EditorExt_1    [A,B,M,S,C,D,V] = new EditorExt.EditorExt_1(e)
  @inline implicit final def ___EditorExt_Tag  [A,B,M[_],S,C,D  ](e: Editor[A,B,M,S,C,D,VdomElement]): EditorExt.EditorExt_Tag  [A,B,M,S,C,D  ] = new EditorExt.EditorExt_Tag(e)
  @inline implicit final def ___EditorExt_IITag[I  ,M[_],S,C,D  ](e: Editor[I,I,M,S,C,D,VdomElement]): EditorExt.EditorExt_IITag[I,  M,S,C,D  ] = new EditorExt.EditorExt_IITag(e)
}

object EditorExt extends EditorExt {

  final class EditorExt_1[A,B,M[_],S,C,D,V](val e: Editor[A,B,M,S,C,D,V]) extends AnyVal {

    def applyCorrection(correctorFn: A => Generic.Corrector[B, _]): Editor[A, B, M, S, C, D, V] =
      e.modCallbacksA { a =>
        val corrector = correctorFn(a)
        _.pmodB {
          case OnChange      (b) => corrector.live(b)
          case OnEditFinished(b) => corrector.applyAndUncorrect(b)
        }
      }

    def applyOnEditFinishedK[K](f: K => ReactST[M, S, Unit])(g: A => K)(implicit M: Monad[M]): Editor[A,B,M,S,C,D,V] =
      e.paddSTA(a => {
        case OnEditFinished(_) => f(g(a))
      })

    def applyRowUpdate[K, P](savedStore: SavedRowStore[S, K, P, B])
                            (k: A => K)
                            (implicit M: Monad[M]): Editor[A, B, M, S, C, D, V] =
      e.modCallbacksA(a =>
        h => h.paddST {
          case OnChange(v)       => savedStore.setIST(k(a), v)
          case OnEditFinished(v) => savedStore.setIST(k(a), v)
        })

    def applyRowUpdateAndRevert[K, P, I](savedStores: NewAndSavedStores[S, K, P, I])
                                        (k: A => Option[K])
                                        (implicit M: Monad[M],
                                         wv: B <:< FieldSet[P, I]#FieldValue,
                                         wf: C <:< FieldSet[P, I]#Field): Editor[A,B,M,S,C,D,V] =
      applyRowUpdateAndRevert(savedStores.s, savedStores.n)(k)

    def applyRowUpdateAndRevert[K, P, I](savedStore: SavedRowStore[S, K, P, I], newStore: NewRowStore[S, I])
                                        (k: A => Option[K])
                                        (implicit M: Monad[M],
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

    def editableByRowStatus(c: StateAccessPure[S])(implicit ev: Callback =:= D, M: M ~> CallbackTo, N: Monad[M]): RowStatus => Option[e.Editable] = {
      val canedit = e.editable(c runState _.st)
      rs => rs match {
        case RowStatus.Sync | RowStatus.Failed(_) => canedit
        case RowStatus.Locked                     => None
      }
    }

  }

  final class EditorExt_Tag[A,B,M[_],S,C,D](val e: Editor[A,B,M,S,C,D,VdomElement]) extends AnyVal {

    def renderOptionalError(f: A => Option[String]): Editor[A,B,M,S,C,D,VdomElement] =
      Editor(i => Editors.renderWithError(e, f(i.data)) render i)

    def wrapInLabel(f: (A, VdomNode) => TagMod): Editor[A,B,M,S,C,D,VdomElement] =
      Editor(i => <.label(f(i.data, e render i)))

    def labelSuffix(f: A => VdomNode): Editor[A,B,M,S,C,D,VdomElement] =
      wrapInLabel((a, i) => TagMod(i, f(a)))

    def applyInputValidation[T, I](invalidate: Simple.Invalidator[A]): Editor[A, B, M, S, C, D, VdomElement] =
      renderOptionalError(invalidate(_).map(Simple.Invalidity.toText))
  }

  final class EditorExt_IITag[I,M[_],S,C,D](val e: Editor[I,I,M,S,C,D,VdomElement]) extends AnyVal {

    def applyValidator(v: Simple.Validator[I, _, _]): Editor[I, I, M, S, C, D, VdomElement] =
      e.applyInputValidation(v.toInvalidator)
        .applyCorrection(_ => v.corrector)

    def applyStatefulValidator[ValidatorState](v: ValidatorState => Simple.Validator[I, _, _]): Editor[(ValidatorState, I), I, M, S, C, D, VdomElement] =
      e.strengthL[ValidatorState]
        .applyInputValidation(Simple.Invalidator(x => v(x._1).toInvalidator(x._2)))
        .applyCorrection(x => v(x._1).corrector)
  }

}
