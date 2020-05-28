package shipreq.base.util

import japgolly.univeq.UnivEq

final case class ErrorMsg(value: String)

object ErrorMsg {

  implicit def univEq: UnivEq[ErrorMsg] =
    UnivEq.derive

  def fromThrowable(t: Throwable): ErrorMsg =
    apply(Option(t.getMessage).getOrElse(t.toString).trim)
}
