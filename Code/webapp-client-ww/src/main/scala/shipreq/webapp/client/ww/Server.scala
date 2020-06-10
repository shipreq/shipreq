package shipreq.webapp.client.ww

import japgolly.scalajs.react.{AsyncCallback, Callback}
import org.scalajs.dom.webworkers.DedicatedWorkerGlobalScope
import shipreq.webapp.client.ww.api.Protocol._

object Server {

  def startDefault[Cmd[_]](service: Service[Cmd],
                           resultEnc: ResultEncoder[Cmd, Codec.default.Writer])
                          (implicit cmdReader: Codec.default.Reader[Cmd[_]]): Callback = {
    import Codec.{default => codec}
    start(codec, service)(interface(codec), resultEnc, OnError.logToConsole)
  }

  def start[Cmd[_]](codec    : Codec,
                    service  : Service[Cmd])
                   (interface: Interface[codec.Encoded],
                    resultEnc: ResultEncoder[Cmd, codec.Writer],
                    onError  : OnError)
                   (implicit cmdReader: codec.Reader[Cmd[_]]): Callback = {

    import codec.Encoded

    def respondForSome[A](id: Int, cmd: Cmd[A]): AsyncCallback[Unit] =
      for {
        a <- service(cmd)
        e  = codec.encode(a)(resultEnc(cmd))
        m  = new Message(id, e)
        _ <- interface.post(m).asAsyncCallback
      } yield ()

    def respond(m: Message[Encoded]): Callback = {
      val cmd = codec.decode[Cmd[_]](m.cmd)
      respondForSome(m.id, cmd).toCallback
    }

    interface.listen(respond, onError)
  }

  // ===================================================================================================================

  trait Service[Cmd[_]] {
    def apply[A](cmd: Cmd[A]): AsyncCallback[A]
  }

  trait ResultEncoder[Cmd[_], W[_]] {
    def apply[A](cmd: Cmd[A]): W[A]
  }

  // ===================================================================================================================

  def interface(codec: Codec): Interface[codec.Encoded] =
    new Interface[codec.Encoded] {
      import codec.Encoded

      private[this] val worker = DedicatedWorkerGlobalScope.self

      override def listen(hnd: Message[Encoded] => Callback, onError: OnError): Callback =
        Callback {
          worker.onmessage = Interface.onMessageFn(hnd)
          worker.onError = Interface.onErrorFn(onError.handle)
          worker.postMessage(Ready)
        }

      override def post(msg: Message[Encoded]): Callback =
        Callback(worker.postMessage(msg, codec.transferables(msg.cmd)))
    }
}