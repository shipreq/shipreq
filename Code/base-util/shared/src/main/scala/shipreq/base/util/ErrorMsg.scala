package shipreq.base.util

import japgolly.univeq.UnivEq

final case class ErrorMsg(value: String)

object ErrorMsg {
  implicit def equality: UnivEq[ErrorMsg] =
    UnivEq.derive
}
