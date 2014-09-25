package shipreq.webapp.client.ui.table

import japgolly.scalajs.react.vdom.ReactVDom.Modifier
import shipreq.webapp.client.ui._
import shipreq.webapp.shared.validation.{ValidatePlusR, ValidatorPlus}

class FieldSpec[P, V, I, C, O](val p2c: P => C, val v: ValidatorPlus[I, C, O], val e: Editor[I, V]) {
  def initial: P => I = v.cp.ci compose p2c
  @inline final def toR[S,R](vw: Option[ValidatePlusR[S, R, O]]) = new FieldSpecR(this, vw)
}

object FieldSpec {
  def apply[P] = new {
    def apply[C](p2c: P => C) = new {
      def apply[I, O](v: ValidatorPlus[I, C, O])(e: Editor[I, Modifier]) =
        new FieldSpec(p2c, v, e)
    }
  }
}

class FieldSpecR[S, R, P, V, I, C, O](s: FieldSpec[P, V, I, C, O], val vr: Option[ValidatePlusR[S, R, O]])
  extends FieldSpec(s.p2c, s.v, s.e)
