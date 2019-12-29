package shipreq.webapp.client.project.app.root

import japgolly.scalajs.react.Reusability
import japgolly.univeq.UnivEq
import shipreq.base.util.IsoBool

sealed trait ConnectionStatus extends IsoBool[ConnectionStatus] {
  override def companion = ConnectionStatus
}

object ConnectionStatus extends IsoBool.Object[ConnectionStatus] {
  override def positive = Connected
  override def negative = Disconnected

  case object Connected extends ConnectionStatus
  case object Disconnected extends ConnectionStatus

  implicit def univEq: UnivEq[ConnectionStatus] = UnivEq.derive
  implicit val reusability: Reusability[ConnectionStatus] = Reusability.by_==
}