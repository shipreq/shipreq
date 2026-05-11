package shipreq.webapp.base.data

import shipreq.base.util.{IsoBool, Valid, Validity}

sealed abstract class Live extends IsoBool.WithBoolOps[Live] {
  override final def companion = Live
}

case object Live extends Live with IsoBool.Object[Live] {
  override def positive = this
  override def negative = Dead
  val whenValid: Validity => Live = fnToThisWhen(Valid)
}

case object Dead extends Live
