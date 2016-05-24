package shipreq.webapp.client.project.protocol

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.Reusability
import shipreq.webapp.base.event.VerifiedEvents
import shipreq.webapp.base.protocol._
import shipreq.webapp.client.base.data.TCB
import shipreq.webapp.client.base.protocol._
import shipreq.webapp.client.project.app.state.ClientData

final class ServerCall[-Input](val fn: (Input, TCB.Success, String => TCB.Failure) => Callback) extends AnyVal {
  @inline def apply(i: Input, s: TCB.Success, f: String => TCB.Failure): Callback =
    fn(i, s, f)
}

object ServerCall {
  @inline def apply[I](fn: (I, TCB.Success, String => TCB.Failure) => Callback): ServerCall[I] =
    new ServerCall(fn)

  def to[I, F <: (I =>|=> VerifiedEvents)](remoteFn: RemoteFn.InstanceFor[F],
                                           cp: ClientProtocol,
                                           cd: ClientData): ServerCall[I] =
    ServerCall((input, onSuccess, onFailure) =>
      cp.call(remoteFn)(
        input,
        s => cd.applyEventsS(s) >> onSuccess,
        f => cp.consumeGenericFailure(f) >> onFailure(cp.genericFailureToText(f))))

  implicit def reusabilityServerCall[I]: Reusability[ServerCall[I]] =
    Reusability.fn((a, b) => a.fn eq b.fn)
}