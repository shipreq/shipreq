package shipreq.webapp.base.protocol

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.Reusability
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.data.TCB

final class ServerSideProcInvoker[-I, +O](val fn: (I, O => TCB.Success, String => TCB.Failure) => Callback) extends AnyVal {
  @inline def apply(input    : I,
                    onSuccess: O => TCB.Success,
                    onFailure: String => TCB.Failure): Callback =
    fn(input, onSuccess, onFailure)
}

object ServerSideProcInvoker {

  def apply[I, O](cp: ClientProtocol, proc: ServerSideProc.Aux[ErrorMsg, I, O]): ServerSideProcInvoker[I, O] =
    new ServerSideProcInvoker((i, s, f) => cp.call(proc)(i, s, _ consumeAnd f))

  implicit def reusability[I, O]: Reusability[ServerSideProcInvoker[I, O]] =
    Reusability((a, b) => a.fn eq b.fn)
}