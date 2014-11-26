package shipreq.webapp.client.util.ui.tablespec2

import monocle.SimpleLens
import shipreq.base.util.ScalaExt._

trait FieldSet[_P, _I] {

  final type P = _P
  final type I = _I

  sealed abstract class Field {
    type V
    def ilens: SimpleLens[I, V]
    def pv: P => V
    @inline final def *(v: V): FieldValue = fieldValue(this)(v)
  }
  protected def field[_V](_pv: P => _V, _ilens: SimpleLens[I, _V]): Field {type V = _V} =
    new Field {
      override type V    = _V
      override def ilens = _ilens
      override def pv    = _pv
    }

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

class FieldSet2[Q, A, B](g1: Q=>A, g2: Q=>B) extends FieldSet[Q, (A,B)] {
  final val f1 = field[A](g1, SimpleLens(_._1, _ put1 _))
  final val f2 = field[B](g2, SimpleLens(_._2, _ put2 _))
  override final val fields   = Vector(f1, f2)
  override final def pi(p: P) = (f1 pv p, f2 pv p)
}
object FieldSet2 {
  def apply[Q] = new {
    @inline def apply[A, B](g1: Q=>A, g2: Q=>B): FieldSet2[Q, A, B] = new FieldSet2(g1, g2)
  }
}