package shipreq.webapp.client.util.ui.tablespec2

import scalaz.{\/, -\/, \/-}
import scalaz.Isomorphism.<=>
import scalaz.Leibniz.===
import shipreq.base.util.ScalaExt._

sealed abstract class EditorCallback[+I] {
  final def map[X](f: I => X): EditorCallback[X] = this match {
    case OnChange(i)       => OnChange(f(i))
    case OnEditFinished(i) => OnEditFinished(f(i))
    case OnCancel          => OnCancel
  }
}
final case class OnChange      [I](input: I) extends EditorCallback[I]
final case class OnEditFinished[I](input: I) extends EditorCallback[I]
case object      OnCancel                    extends EditorCallback[Nothing]

// =====================================================================================================================

case class EditorCallbacks[I, C, D](t: (EditorCallback[I], C) => D) {
  @inline final def apply(i: EditorCallback[I], c: C): D = t(i, c)

  def cmap [X,Y]  (f: X => I, g: Y => C)            = EditorCallbacks[X, Y, D]((x,c) => t(x map f, g(c)))
  def cmapC[X]    (f: X => C)                       = EditorCallbacks[I, X, D]((x,c) => t(x, f(c)))
  def map  [X]    (f: D => X)                       = EditorCallbacks[I, C, X](t andThenA f)
  def dimap[X,Y,Z](f: X => I, g: Y => C, h: D => Z) = EditorCallbacks[X, Y, Z]((x,c) => h(t(x map f, g(c))))

  def pmodI(f: C => PartialFunction[EditorCallback[I], I]): EditorCallbacks[I, C, D] =
    EditorCallbacks((i,c) => {
      val j = f(c).andThen(i2 => i map (_ => i2)).applyOrElse(i, identity[EditorCallback[I]])
      t(j, c)
    })

  def pmodC(f: C => PartialFunction[EditorCallback[I], C]): EditorCallbacks[I, C, D] =
    EditorCallbacks((i,c) => {
      val c2 = f(c).applyOrElse(i, (_: Any) => c)
      t(i, c2)
    })
}

// =====================================================================================================================

case class EditorInput[A, B, C, D](data: A, cssClass: String,
                                   editable: Option[EditorCallbacks[B, C, D]]) {

  def mapABC[X,Y,Z](f: A => X, g: Y => B, h: Z => C): EditorInput[X,Y,Z,D] =
    copy(data = f(data), editable = editable map (_.cmap(g, h)))

  def mapA [X](f: A => X): EditorInput[X, B, C, D] = copy(data = f(data))
  def cmapB[X](f: X => B): EditorInput[A, X, C, D] = mapCallbacks(_.cmap(f, identity))
  def cmapC[X](f: X => C): EditorInput[A, B, X, D] = mapCallbacks(_ cmapC f)
  def mapD [X](f: D => X): EditorInput[A, B, C, X] = mapCallbacks(_ map f)

  def mapCallbacks[X,Y,Z](f: EditorCallbacks[B,C,D] => EditorCallbacks[X,Y,Z]): EditorInput[A,X,Y,Z] =
    copy(editable = editable map f)
}

// =====================================================================================================================

case class Editor[A, B, C, D, V](render: EditorInput[A, B, C, D] => V) {
  def strengthL[X]                      : Editor[(X,A), B, C, D, V] = cmapA(_._2)
  def strengthR[X]                      : Editor[(A,X), B, C, D, V] = cmapA(_._1)
  def cmapA    [X]  (f: X ⇒ A)          : Editor[X,     B, C, D, V] = Editor(i ⇒ render(i mapA f))
  def mapB     [X]  (f: B ⇒ X)          : Editor[A,     X, C, D, V] = Editor(i ⇒ render(i cmapB f))
  def mapC     [X]  (f: C ⇒ X)          : Editor[A,     B, X, D, V] = Editor(i ⇒ render(i cmapC f))
  def cmapD    [X]  (f: X ⇒ D)          : Editor[A,     B, C, X, V] = Editor(i ⇒ render(i mapD f))
  def xmap     [X,Y](f: X ⇒ A, g: B ⇒ Y): Editor[X,     Y, C, D, V] = Editor(i ⇒ render(i.mapABC[A, B, C](f, g, identity)))

  def imap[X](i: A <=> X)(implicit ev: A === B): Editor[X, X, C, D, V] =
    xmap[X, X](i.from, ev.subst[({type λ[α] = α => X})#λ](i.to))

  def pmodBx(f: A ⇒ C ⇒ PartialFunction[EditorCallback[B], B]): Editor[A, B, C, D, V] = cmapCallbacksA[B,C,D](a ⇒ _ pmodI f(a))
  def pmodCx(f: A ⇒ C ⇒ PartialFunction[EditorCallback[B], C]): Editor[A, B, C, D, V] = cmapCallbacksA[B,C,D](a ⇒ _ pmodC f(a))
  def pmodB (f:         PartialFunction[EditorCallback[B], B]): Editor[A, B, C, D, V] = cmapCallbacks[B,C,D](_ pmodI (_ => f))
  def pmodC (f:         PartialFunction[EditorCallback[B], C]): Editor[A, B, C, D, V] = cmapCallbacks[B,C,D](_ pmodC (_ => f))
  // TODO pmodC ↑ looks like it discards old Cs instead of appending

  def cmapCallbacksA[X,Y,Z](f: A => EditorCallbacks[X,Y,Z] => EditorCallbacks[B,C,D]): Editor[A,X,Y,Z,V] = Editor(i => render(i.mapCallbacks(f(i.data))))
  def cmapCallbacks [X,Y,Z](f: EditorCallbacks[X,Y,Z]      => EditorCallbacks[B,C,D]): Editor[A,X,Y,Z,V] = Editor(i => render(i mapCallbacks f))

  @inline def modCallbacksA(f: A => EditorCallbacks[B, C, D] => EditorCallbacks[B, C, D]): Editor[A, B, C, D, V] = cmapCallbacksA(f)
  @inline def modCallbacks(f: EditorCallbacks[B, C, D] => EditorCallbacks[B, C, D]): Editor[A, B, C, D, V] = cmapCallbacks(f)

  def compose[M, N, O>:C, P<:D, Q](t: Editor[M,N,O,P,Q]): Editor[(A,M), B\/N, O, P, (V,Q)] =
    Editor[(A,M), B\/N, O, P, (V,Q)](i => {
      val i1 = i.copy[A,B,C,D](data = i.data._1, editable = i.editable.map(_.dimap[B,C,D](-\/.apply, c⇒c, d⇒d)))
      val i2 = i.copy[M,N,O,P](data = i.data._2, editable = i.editable.map(_.dimap[N,O,P](\/-.apply, o⇒o, p⇒p)))
      (this render i1, t render i2)
    })
}

// =====================================================================================================================

object Editor {
  type F = FieldSet[_, _]
  def merge2[C, D, A1, B1, V1, A2, B2, V2, FS <: FieldSet2[_, B1, B2]](fs: FS, e1: Editor[A1, B1, C, D, V1], e2: Editor[A2, B2, C, D, V2]) =
  new {
    def tupleI[C2](g: C => C2) = apply[(A1, A2), C2](_._1, _._2, g)
    def apply[I, C2](a1: I => A1, a2: I => A2, g: C => C2): Editor[I, fs.FieldValue, (fs.Field, C2), D, (V1, V2)] =
      Editor(ei => {
        val i1 = ei.mapABC[A1, B1, C](a1, fs.f1 * _, c => (fs.f1, g(c)))
        val i2 = ei.mapABC[A2, B2, C](a2, fs.f2 * _, c => (fs.f2, g(c)))
        (e1 render i1, e2 render i2)
      })
  }

  def merge3[C, D, A1, B1, V1, A2, B2, V2, A3, B3, V3, FS <: FieldSet3[_, B1, B2, B3]](fs: FS, e1: Editor[A1, B1, C, D, V1], e2: Editor[A2, B2, C, D, V2], e3: Editor[A3, B3, C, D, V3]) =
    new {
    def tupleI[C2](g: C => C2) = apply[(A1, A2, A3), C2](_._1, _._2, _._3, g)
    def apply[I, C2](a1: I => A1, a2: I => A2, a3: I => A3, g: C => C2): Editor[I, fs.FieldValue, (fs.Field, C2), D, (V1, V2, V3)] =
      Editor(ei => {
        val i1 = ei.mapABC[A1, B1, C](a1, fs.f1 * _, c => (fs.f1, g(c)))
        val i2 = ei.mapABC[A2, B2, C](a2, fs.f2 * _, c => (fs.f2, g(c)))
        val i3 = ei.mapABC[A3, B3, C](a3, fs.f3 * _, c => (fs.f3, g(c)))
        (e1 render i1, e2 render i2, e3 render i3)
      })
  }

  def merge3S[S, C, D, A1, B1, V1, A2, B2, V2, A3, B3, V3, FS <: FieldSet3[_, B1, B2, B3]](fs: FS, e1: Editor[(S,A1), B1, C, D, V1], e2: Editor[(S,A2), B2, C, D, V2], e3: Editor[(S,A3), B3, C, D, V3]) =
    new {
      def tupleI[C2](g: C => C2) = apply[(A1, A2, A3), C2](_._1, _._2, _._3, g)
      def apply[I, C2](a1: I => A1, a2: I => A2, a3: I => A3, g: C => C2): Editor[(S,I), fs.FieldValue, (fs.Field, C2), D, (V1, V2, V3)] =
        merge3(fs,e1,e2,e3).apply[(S,I),C2](_ map2 a1, _ map2 a2, _ map2 a3, g)
    }
}