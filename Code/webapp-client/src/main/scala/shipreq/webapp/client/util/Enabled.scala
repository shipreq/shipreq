package shipreq.webapp.client.util

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.IsoBool

sealed trait Enabled

case object Enabled extends Enabled with IsoBool.Obj[Enabled] {
  override protected def neg = Disabled
  implicit val reusability = Reusability.byEqual[Enabled]
}

case object Disabled extends Enabled with IsoBool[Enabled] {
  override protected def neg = Enabled
}