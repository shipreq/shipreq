package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.microlibs.stdlib_ext.StdlibExt._
import monocle.{Iso, Lens}

trait FieldSet[_P, _I] {

  final type P = _P
  final type I = _I

  sealed abstract class Field {
    type V
    def ilens: Lens[I, V]
    def pv: P => V
    @inline final def *(v: V): FieldValue = fieldValue(this)(v)
  }
  protected def field[_V](_pv: P => _V, _ilens: Lens[I, _V]): Field {type V = _V} =
    new Field {
      override type V    = _V
      override def ilens = _ilens
      override def pv    = _pv
    }

  val emptyI: I
  val fields: Vector[Field]
  def pi(p: P): I

  sealed trait FieldValue {
    val f: Field
    val v: f.V
  }

  private def fieldValue(a: Field)(b: a.V): FieldValue =
    new FieldValue {
      override final val f: a.type = a
      override final val v = b
    }
}

class FieldSet1[Q, A](g1: Q=>A, i: A) extends FieldSet[Q, A] {
  final val f1 = field[A](g1, Iso.id.asLens)
  override final val emptyI   = i
  override final val fields   = Vector(f1)
  override final def pi(p: P) = (f1 pv p)
}
object FieldSet1 {
  def apply[Q] = new {
    @inline def apply[A](g1: Q=>A)(emptyI: A): FieldSet1[Q, A] = new FieldSet1(g1, emptyI)
  }
}

class FieldSet2[Q, A, B](g1: Q=>A, g2: Q=>B, i: (A,B)) extends FieldSet[Q, (A,B)] {
  private type T = (A,B)
  final val f1 = field[A](g1, Lens((_: T)._1)(v => _ put1 v))
  final val f2 = field[B](g2, Lens((_: T)._2)(v => _ put2 v))
  override final val emptyI   = i
  override final val fields   = Vector(f1, f2)
  override final def pi(p: P) = (f1 pv p, f2 pv p)
}
object FieldSet2 {
  def apply[Q] = new {
    @inline def apply[A, B](g1: Q=>A, g2: Q=>B)(emptyI: (A,B)): FieldSet2[Q, A, B] = new FieldSet2(g1, g2, emptyI)
  }
}

class FieldSet3[Q, A, B, C](g1: Q=>A, g2: Q=>B, g3: Q=>C, i: (A,B,C)) extends FieldSet[Q, (A,B,C)] {
  private type T = (A,B,C)
  final val f1 = field[A](g1, Lens((_: T)._1)(v => _ put1 v))
  final val f2 = field[B](g2, Lens((_: T)._2)(v => _ put2 v))
  final val f3 = field[C](g3, Lens((_: T)._3)(v => _ put3 v))
  override final val emptyI   = i
  override final val fields   = Vector(f1, f2, f3)
  override final def pi(p: P) = (f1 pv p, f2 pv p, f3 pv p)
}
object FieldSet3 {
  def apply[Q] = new {
    @inline def apply[A, B, C](g1: Q=>A, g2: Q=>B, g3: Q=>C)(emptyI: (A,B,C)): FieldSet3[Q, A, B, C] = new FieldSet3(g1, g2, g3, emptyI)
  }
}

class FieldSet4[Q, A, B, C, D](g1: Q=>A, g2: Q=>B, g3: Q=>C, g4: Q=>D, i: (A,B,C,D)) extends FieldSet[Q, (A,B,C,D)] {
  private type T = (A,B,C,D)
  final val f1 = field[A](g1, Lens((_: T)._1)(v => _ put1 v))
  final val f2 = field[B](g2, Lens((_: T)._2)(v => _ put2 v))
  final val f3 = field[C](g3, Lens((_: T)._3)(v => _ put3 v))
  final val f4 = field[D](g4, Lens((_: T)._4)(v => _ put4 v))
  override final val emptyI   = i
  override final val fields   = Vector(f1, f2, f3, f4)
  override final def pi(p: P) = (f1 pv p, f2 pv p, f3 pv p, f4 pv p)
}
object FieldSet4 {
  def apply[Q] = new {
    @inline def apply[A, B, C, D](g1: Q=>A, g2: Q=>B, g3: Q=>C, g4: Q=>D)(emptyI: (A,B,C,D)): FieldSet4[Q, A, B, C, D] = new FieldSet4(g1, g2, g3, g4, emptyI)
  }
}