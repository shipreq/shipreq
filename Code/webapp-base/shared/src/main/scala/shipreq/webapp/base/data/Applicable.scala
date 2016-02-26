package shipreq.webapp.base.data

import shipreq.base.util.IsoBool

sealed trait Applicable extends IsoBool.WithBoolOps[Applicable] {
  override final def companion = Applicable
}

case object Applicable extends Applicable with IsoBool.Object[Applicable] {
  override def positive = Applicable
  override def negative = NotApplicable
}

case object NotApplicable extends Applicable
