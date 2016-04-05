package shipreq.webapp.client.data

import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.IsoBool
import shipreq.webapp.client.lib.DataReusability._

/** Is a subject on or off? */
sealed trait On extends IsoBool.WithBoolOps[On] {
  override final def companion = On
}

case object On extends On with IsoBool.Object[On] {
  override def positive = On
  override def negative = Off
  implicit val reusability = Reusability.byUnivEq[On]
}

case object Off extends On