package shipreq.webapp.client.data

import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.IsoBool
import shipreq.webapp.client.lib.DataReusability._

sealed trait Enabled extends IsoBool[Enabled] {
  override final def companion = Enabled
}

case object Enabled extends Enabled with IsoBool.Object[Enabled] {
  override def positive = Enabled
  override def negative = Disabled
  implicit val reusability = Reusability.byUnivEq[Enabled]
}

case object Disabled extends Enabled