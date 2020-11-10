package shipreq.webapp.member.protocol.webworker

import japgolly.scalajs.react.Callback
import shipreq.base.util.ErrorMsg

final case class OnError(handle: ErrorMsg => Callback) extends AnyVal {
  @inline def apply(e: ErrorMsg) = handle(e)
}

object OnError {
  def logToConsole: OnError =
    OnError(err => Callback(console.error(err)))
}
