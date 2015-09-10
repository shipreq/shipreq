package shipreq.webapp.client.util

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.IsoBool

/** Is a subject on or off? */
sealed trait On

case object On extends On with IsoBool.Obj[On] {
  override protected def neg = Off
  implicit val reusability = Reusability.byEqual[On]
}

case object Off extends On with IsoBool[On] {
  override protected def neg = On
}