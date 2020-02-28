package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.ScalazReact._
import scalaz._
import scalaz.Isomorphism.<=>
import scalaz.Leibniz.===

sealed abstract class CallbackEvent[+I] {
  final def map[X](f: I => X): CallbackEvent[X] = this match {
    case OnChange(i)       => OnChange(f(i))
    case OnEditFinished(i) => OnEditFinished(f(i))
    case OnCancel          => OnCancel
  }
}
final case class OnChange      [I](input: I) extends CallbackEvent[I]
final case class OnEditFinished[I](input: I) extends CallbackEvent[I]
case object      OnCancel                    extends CallbackEvent[Nothing]

// =====================================================================================================================

// H for Handler
final case class CallbackH[B, M[_], S, C](event: CallbackEvent[B], st: ReactST[M, S, Unit], data: C) {
  type ST = ReactST[M, S, Unit]
  type Self = CallbackH[B, M, S, C]

  def mapB [X]     (f: B => X)                   : CallbackH[X, M, S, C] = copy(event = event map f)
  def mapBC[X,Y]   (f: B => X, g: C => Y)        : CallbackH[X, M, S, Y] = copy(event = event map f, data = g(data))
  def mapST[N[_],T](f: ST => ReactST[N, T, Unit]): CallbackH[B, N, T, C] = copy(st = f(st))

  def pmodB(pf: PartialFunction[CallbackEvent[B], B]): Self =
    pmod(pf)(b => copy(event = this.event.map(_ => b)))

  def paddST(pf: PartialFunction[CallbackEvent[B], ReactST[M, S, Unit]])(implicit M: Monad[M]): CallbackH[B, M, S, C] =
    pmod(pf)(t => copy(st = this.st >> t))

  private[this] def pmod[X](f: PartialFunction[CallbackEvent[B], X])(g: X => Self): Self =
    f.andThen(g).applyOrElse(event, (_: CallbackEvent[B]) => this)
}

// =====================================================================================================================

// I for Input
final case class EditorI[A, B, M[_], S, C, D](data: A, cssClass: String,
                                              editable: Option[CallbackH[B, M, S, C] => D]) {

  type CBH = CallbackH[B, M, S, C]

  def addCssClass(c: String): EditorI[A,B,M,S,C,D] =
    copy(cssClass = if (cssClass.isEmpty) c else s"$cssClass $c")

  def mapAB[X,Y](f: A => X, g: Y => B): EditorI[X,Y,M,S,C,D] =
    copy(data = f(data), editable = cmapcbh(_ mapB g))

  def mapABC[X,Y,Z](f: A => X, g: Y => B, h: Z => C): EditorI[X,Y,M,S,Z,D] =
    copy(data = f(data), editable = cmapcbh(_.mapBC(g,h)))
//
  def mapA [X](f: A => X): EditorI[X, B, M, S, C, D] = copy(data = f(data))
//  def cmapB[X](f: X => B): EditorI[A, X, M, S, C, D] = mapCallbacks(_.cmap(f, identity))
//  def cmapC[X](f: X => C): EditorI[A, B, M, S, X, D] = mapCallbacks(_ cmapC f)
//  def mapD [X](f: D => X): EditorI[A, B, M, S, C, X] = mapCallbacks(_ map f)

  private def cmapcbh[X, N[_], T, Y](f: CallbackH[X,N,T,Y] => CallbackH[B,M,S,C]): Option[CallbackH[X, N, T, Y] => D] =
    editable.map[CallbackH[X,N,T,Y] => D](g => h => g(f(h)))

  def cmapCallbackH[X, N[_], T, Y](f: CallbackH[X,N,T,Y] => CallbackH[B,M,S,C]): EditorI[A, X, N, T, Y, D] =
    copy(editable = cmapcbh(f))

  def modCallbackH(f: CallbackH[B,M,S,C] => CallbackH[B,M,S,C]): EditorI[A, B, M, S, C, D] = {
    val e2 = editable.map[CallbackH[B,M,S,C] => D](g => h => g(f(h)))
    if (editable eq e2)
      this
    else
      copy(editable = e2)
  }
}

// =====================================================================================================================

/**
 * @tparam A Input value to the editor
 * @tparam B Output value from the editor
 * @tparam M Callback state monad context.
 * @tparam S Callback state monad state.
 * @tparam C Supplementary info about the editor. Used to identity fields when editors are merged.
 * @tparam D Callbacks in their final, computed state, ready to be used directly by the editor.
 * @tparam V The final, rendered editor type.
 */
final case class Editor[A, B, M[_], S, C, D, V](render: EditorI[A, B, M, S, C, D] => V) {
  type _A = A
  type _B = B
  type _S = S
  type _C = C
  type _D = D
  type _V = V
  type Input     = EditorI[A, B, M, S, C, D]
  type InputA    = A
  type View      = V
  type CBH       = CallbackH[B, M, S, C]
  type Editable = CBH => D
  def editable(f: CBH => D): Option[Editable] = Some(f)

  def strengthL[X]                      : Editor[(X,A), B, M, S, C, D, V] = cmapA(_._2)
  def strengthR[X]                      : Editor[(A,X), B, M, S, C, D, V] = cmapA(_._1)
  def cmapA    [X]  (f: X ⇒ A)          : Editor[X,     B, M, S, C, D, V] = Editor(i ⇒ render(i mapA f))
//  def mapB     [X]  (f: B ⇒ X)          : Editor[A,     X, M, S, C, D, V] = Editor(i ⇒ render(i cmapB f))
//  def mapC     [X]  (f: C ⇒ X)          : Editor[A,     B, M, S, X, D, V] = Editor(i ⇒ render(i cmapC f))
//  def cmapD    [X]  (f: X ⇒ D)          : Editor[A,     B, M, S, C, X, V] = Editor(i ⇒ render(i mapD f))
  def xmap     [X,Y](f: X ⇒ A, g: B ⇒ Y): Editor[X,     Y, M, S, C, D, V] = Editor(i ⇒ render(i.mapAB(f, g)))

  def imap[X](i: A <=> X)(implicit ev: A === B): Editor[X, X, M, S, C, D, V] =
    xmap[X, X](i.from, ev.subst[({type λ[α] = α => X})#λ](i.to))

  def mapCallbacksA[X, N[_], T, Y](f: A => CallbackH[B, M, S, C] => CallbackH[X, N, T, Y]): Editor[A,X,N,T,Y,D,V] =
    Editor(i => render(i.cmapCallbackH(f(i.data))))
  def mapCallbacks[X, N[_], T, Y](f: CallbackH[B, M, S, C] => CallbackH[X, N, T, Y]): Editor[A,X,N,T,Y,D,V] =
    Editor(i => render(i.cmapCallbackH(f)))

  def modCallbacksA(f: A => CallbackH[B, M, S, C] => CallbackH[B, M, S, C]): Editor[A,B,M,S,C,D,V] =
    Editor(i => render(i.modCallbackH(f(i.data))))
  def modCallbacks(f: CallbackH[B, M, S, C] => CallbackH[B, M, S, C]): Editor[A,B,M,S,C,D,V] =
    Editor(i => render(i.modCallbackH(f)))

  def pmodB(pf: PartialFunction[CallbackEvent[B], B]): Editor[A,B,M,S,C,D,V] =
    modCallbacks(_.pmodB(pf))

  def paddST(pf: PartialFunction[CallbackEvent[B], ReactST[M, S, Unit]])(implicit M: Monad[M]): Editor[A,B,M,S,C,D,V] =
    modCallbacks(_.paddST(pf))

  def paddSTA(pf: A => PartialFunction[CallbackEvent[B], ReactST[M, S, Unit]])(implicit M: Monad[M]): Editor[A,B,M,S,C,D,V] =
    modCallbacksA(a => _.paddST(pf(a)))

  def zoomU[T](implicit M: Monad[M], ev: S === Unit): Editor[A,B,M,T,C,D,V] =
    mapCallbacks(_.mapST(_.zoomU[T]))

//  def compose[M, N, O>:C, P<:D, Q](t: Editor[M,N,O,P,Q]): Editor[(A,M), B\/N, O, P, (V,Q)] =
//    Editor[(A,M), B\/N, O, P, (V,Q)](i => {
//      val i1 = i.copy[A,B,C,D](data = i.data._1, editable = i.editable.map(_.dimap[B,C,D](-\/.apply, c⇒c, d⇒d)))
//      val i2 = i.copy[M,N,O,P](data = i.data._2, editable = i.editable.map(_.dimap[N,O,P](\/-.apply, o⇒o, p⇒p)))
//      (this render i1, t render i2)
//    })

  def addCssClass(c: String): Editor[A,B,M,S,C,D,V] =
    Editor(i => render(i addCssClass c))
}

// =====================================================================================================================

object Editor {
  import scala.language.reflectiveCalls

  // Generated by bin/gen-editor

  def merge1[M[_], S, D, A1,B1,V1, FS <: FieldSet1[_,B1]](fs: FS, e1: Editor[A1,B1,M,S,Unit,D,V1]) =
    new {
      def tupleI = apply[A1](identity)
      def apply[I](a1: I⇒A1): Editor[I, fs.FieldValue, M, S, fs.Field, D, (V1)] =
        Editor(ei ⇒ {
          val i1 = ei.mapABC[A1,B1,Unit](a1, fs.f1 * _, _ ⇒ fs.f1)
          (e1 render i1)
        })
    }

  def merge1S[M[_], T, S, C, D, A1,B1,V1, FS <: FieldSet1[_,B1]](fs: FS, e1: Editor[(T,A1),B1,M,S,Unit,D,V1]) =
    new {
      def tupleI = apply[A1](identity)
      def apply[I](a1: I⇒A1): Editor[(T,I), fs.FieldValue, M, S, fs.Field, D, (V1)] =
        merge1(fs,e1).apply[(T,I)](_ map2 a1)
    }


  def merge2[M[_], S, D, A1,B1,V1, A2,B2,V2, FS <: FieldSet2[_,B1,B2]](fs: FS, e1: Editor[A1,B1,M,S,Unit,D,V1], e2: Editor[A2,B2,M,S,Unit,D,V2]) =
    new {
      def tupleI = apply[(A1,A2)](_._1,_._2)
      def apply[I](a1: I⇒A1, a2: I⇒A2): Editor[I, fs.FieldValue, M, S, fs.Field, D, (V1,V2)] =
        Editor(ei ⇒ {
          val i1 = ei.mapABC[A1,B1,Unit](a1, fs.f1 * _, _ ⇒ fs.f1)
          val i2 = ei.mapABC[A2,B2,Unit](a2, fs.f2 * _, _ ⇒ fs.f2)
          (e1 render i1, e2 render i2)
        })
    }

  def merge2S[M[_], T, S, C, D, A1,B1,V1, A2,B2,V2, FS <: FieldSet2[_,B1,B2]](fs: FS, e1: Editor[(T,A1),B1,M,S,Unit,D,V1], e2: Editor[(T,A2),B2,M,S,Unit,D,V2]) =
    new {
      def tupleI = apply[(A1,A2)](_._1,_._2)
      def apply[I](a1: I⇒A1, a2: I⇒A2): Editor[(T,I), fs.FieldValue, M, S, fs.Field, D, (V1,V2)] =
        merge2(fs,e1,e2).apply[(T,I)](_ map2 a1, _ map2 a2)
    }


  def merge3[M[_], S, D, A1,B1,V1, A2,B2,V2, A3,B3,V3, FS <: FieldSet3[_,B1,B2,B3]](fs: FS, e1: Editor[A1,B1,M,S,Unit,D,V1], e2: Editor[A2,B2,M,S,Unit,D,V2], e3: Editor[A3,B3,M,S,Unit,D,V3]) =
    new {
      def tupleI = apply[(A1,A2,A3)](_._1,_._2,_._3)
      def apply[I](a1: I⇒A1, a2: I⇒A2, a3: I⇒A3): Editor[I, fs.FieldValue, M, S, fs.Field, D, (V1,V2,V3)] =
        Editor(ei ⇒ {
          val i1 = ei.mapABC[A1,B1,Unit](a1, fs.f1 * _, _ ⇒ fs.f1)
          val i2 = ei.mapABC[A2,B2,Unit](a2, fs.f2 * _, _ ⇒ fs.f2)
          val i3 = ei.mapABC[A3,B3,Unit](a3, fs.f3 * _, _ ⇒ fs.f3)
          (e1 render i1, e2 render i2, e3 render i3)
        })
    }

  def merge3S[M[_], T, S, C, D, A1,B1,V1, A2,B2,V2, A3,B3,V3, FS <: FieldSet3[_,B1,B2,B3]](fs: FS, e1: Editor[(T,A1),B1,M,S,Unit,D,V1], e2: Editor[(T,A2),B2,M,S,Unit,D,V2], e3: Editor[(T,A3),B3,M,S,Unit,D,V3]) =
    new {
      def tupleI = apply[(A1,A2,A3)](_._1,_._2,_._3)
      def apply[I](a1: I⇒A1, a2: I⇒A2, a3: I⇒A3): Editor[(T,I), fs.FieldValue, M, S, fs.Field, D, (V1,V2,V3)] =
        merge3(fs,e1,e2,e3).apply[(T,I)](_ map2 a1, _ map2 a2, _ map2 a3)
    }

  def merge4[M[_], S, D, A1,B1,V1, A2,B2,V2, A3,B3,V3, A4,B4,V4, FS <: FieldSet4[_,B1,B2,B3,B4]](fs: FS, e1: Editor[A1,B1,M,S,Unit,D,V1], e2: Editor[A2,B2,M,S,Unit,D,V2], e3: Editor[A3,B3,M,S,Unit,D,V3], e4: Editor[A4,B4,M,S,Unit,D,V4]) =
    new {
    def tupleI = apply[(A1,A2,A3,A4)](_._1,_._2,_._3,_._4)
    def apply[I](a1: I⇒A1, a2: I⇒A2, a3: I⇒A3, a4: I⇒A4): Editor[I, fs.FieldValue, M, S, fs.Field, D, (V1,V2,V3,V4)] =
      Editor(ei ⇒ {
        val i1 = ei.mapABC[A1,B1,Unit](a1, fs.f1 * _, _ ⇒ fs.f1)
        val i2 = ei.mapABC[A2,B2,Unit](a2, fs.f2 * _, _ ⇒ fs.f2)
        val i3 = ei.mapABC[A3,B3,Unit](a3, fs.f3 * _, _ ⇒ fs.f3)
        val i4 = ei.mapABC[A4,B4,Unit](a4, fs.f4 * _, _ ⇒ fs.f4)
        (e1 render i1, e2 render i2, e3 render i3, e4 render i4)
      })
  }

  def merge4S[M[_], T, S, C, D, A1,B1,V1, A2,B2,V2, A3,B3,V3, A4,B4,V4, FS <: FieldSet4[_,B1,B2,B3,B4]](fs: FS, e1: Editor[(T,A1),B1,M,S,Unit,D,V1], e2: Editor[(T,A2),B2,M,S,Unit,D,V2], e3: Editor[(T,A3),B3,M,S,Unit,D,V3], e4: Editor[(T,A4),B4,M,S,Unit,D,V4]) =
    new {
      def tupleI = apply[(A1,A2,A3,A4)](_._1,_._2,_._3,_._4)
      def apply[I](a1: I⇒A1, a2: I⇒A2, a3: I⇒A3, a4: I⇒A4): Editor[(T,I), fs.FieldValue, M, S, fs.Field, D, (V1,V2,V3,V4)] =
        merge4(fs,e1,e2,e3,e4).apply[(T,I)](_ map2 a1, _ map2 a2, _ map2 a3, _ map2 a4)
    }
}