package shipreq.webapp.base.data

import japgolly.univeq.UnivEq

final case class Username(value: String) extends AnyVal {
  def with_@ : String =
    "@" + value
}

object Username {
  implicit def univEq: UnivEq[Username] = UnivEq.derive
}