package shipreq.webapp.client.util.ui.table

import shipreq.webapp.client.util.ui._
import shipreq.webapp.base.validation.{ValidatePlusR, ValidatorPlus}
import scalaz.Isomorphism.<=>

class FieldSpec[P, V, I, C, O](val p2c: P => C, val v: ValidatorPlus[I, C, O], val e: Editor[I, V]) {
  def initial: P => I = v.cp.ci compose p2c
  @inline final def toR[S,R](vw: Option[ValidatePlusR[S, R, O]]) = new FieldSpecR(this, vw)
}

object FieldSpec {
  @inline final def apply[P] = new FieldSpecB[P]
  final class FieldSpecB[P] {

    @inline def apply[C](p2c: P => C) = new B2(p2c)
    final class B2[C](p2c: P => C) {
      @inline def apply[I, O](v: ValidatorPlus[I, C, O]) = new NeedEditor(p2c, v)
    }

    @inline def noValidation[I, O](p2o: P => O, iso: I <=> O): NeedEditor[P, I, I, O] =
      new NeedEditor(iso.from compose p2o, ValidatorPlus nop iso.to)
  }

  final class NeedEditor[P, I, C, O](p2c: P => C, v: ValidatorPlus[I, C, O]) {
    @inline def apply[V](e: Editor[I, V]) = new FieldSpec(p2c, v, e)
  }
}

class FieldSpecR[S, R, P, V, I, C, O](s: FieldSpec[P, V, I, C, O], val vr: Option[ValidatePlusR[S, R, O]])
  extends FieldSpec(s.p2c, s.v, s.e)
