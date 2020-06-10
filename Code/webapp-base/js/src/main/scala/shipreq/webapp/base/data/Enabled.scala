package shipreq.webapp.base.data

import shipreq.base.util.IsoBool

sealed trait Enabled extends IsoBool.WithBoolOps[Enabled] {
  override final def companion = Enabled
}

case object Enabled extends Enabled with IsoBool.Object[Enabled] {
  override def positive = Enabled
  override def negative = Disabled
}

case object Disabled extends Enabled