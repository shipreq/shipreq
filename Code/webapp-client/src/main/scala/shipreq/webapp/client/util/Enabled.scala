package shipreq.webapp.client.util

import shipreq.base.util.IsoBool

sealed trait Enabled

case object Enabled extends Enabled with IsoBool.Obj[Enabled] {
  override protected def neg = Disabled
}

case object Disabled extends Enabled with IsoBool[Enabled] {
  override protected def neg = Enabled
}