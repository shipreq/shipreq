package shipreq.webapp.client.util.ui.tablespec2

import scalaz.{\/, -\/, \/-}
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
  def strengthL[X]           : Editor[(X,A), B, C, D, V] = cmapA(_._2)
  def strengthR[X]           : Editor[(A,X), B, C, D, V] = cmapA(_._1)
  def cmapA    [X](f: X => A): Editor[X,     B, C, D, V] = Editor(i => render(i mapA f))
  def mapB     [X](f: B => X): Editor[A,     X, C, D, V] = Editor(i => render(i cmapB f))
  def mapC     [X](f: C => X): Editor[A,     B, X, D, V] = Editor(i => render(i cmapC f))
  def cmapD    [X](f: X => D): Editor[A,     B, C, X, V] = Editor(i => render(i mapD f))

  def pmodBx(f: A => C => PartialFunction[EditorCallback[B], B]): Editor[A, B, C, D, V] = cmapCallbacksA[B,C,D](a ⇒ _ pmodI f(a))
  def pmodCx(f: A => C => PartialFunction[EditorCallback[B], C]): Editor[A, B, C, D, V] = cmapCallbacksA[B,C,D](a ⇒ _ pmodC f(a))

  def pmodB(f: PartialFunction[EditorCallback[B], B]): Editor[A, B, C, D, V] = cmapCallbacks[B,C,D](_ pmodI (_ => f))
  def pmodC(f: PartialFunction[EditorCallback[B], C]): Editor[A, B, C, D, V] = cmapCallbacks[B,C,D](_ pmodC (_ => f))

  def cmapCallbacksA[X,Y,Z](f: A => EditorCallbacks[X,Y,Z] => EditorCallbacks[B,C,D]): Editor[A,X,Y,Z,V] = Editor(i => render(i.mapCallbacks(f(i.data))))
  def cmapCallbacks [X,Y,Z](f: EditorCallbacks[X,Y,Z]      => EditorCallbacks[B,C,D]): Editor[A,X,Y,Z,V] = Editor(i => render(i mapCallbacks f))

  @inline def modCallbacksA(f: A => EditorCallbacks[B, C, D] => EditorCallbacks[B, C, D]): Editor[A, B, C, D, V] = cmapCallbacksA(f)

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
  def merge2[C, D, A1, B1, V1, A2, B2, V2, FS <: FieldSet2[_, B1, B2]](fs: FS, e1: Editor[A1, B1, C, D, V1], e2: Editor[A2, B2, C, D, V2]) = new {
    def pairI = apply[(A1, A2)](_._1, _._2)
    def apply[I](a1: I => A1, a2: I => A2): Editor[I, fs.FieldValue, (fs.Field, C), D, (V1, V2)] =
      Editor(ei => {
        val i1 = ei.mapABC[A1, B1, C](a1, fs.f1 * _, (fs.f1, _))
        val i2 = ei.mapABC[A2, B2, C](a2, fs.f2 * _, (fs.f2, _))
        (e1 render i1, e2 render i2)
      })
  }
}