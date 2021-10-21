package shipreq.webapp.client.ww

import japgolly.scalajs.react.{AsyncCallback, Callback}
import org.scalajs.dom.DedicatedWorkerGlobalScope
import scala.util.{Failure, Success}
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.client.ww.api.Protocol._

object Server {

  def startDefault[Cmd[_]](service  : Service[Cmd],
                           resultEnc: ResultEncoder[Cmd, Codec.default.Writer],
                           logger   : LoggerJs)
                          (implicit cmdReader: Codec.default.Reader[Cmd[_]]): Callback = {
    import Codec.{default => codec}
    start(codec, service)(interface(codec), resultEnc, logger, OnError.logToConsole)
  }

  def start[Cmd[_]](codec    : Codec,
                    service  : Service[Cmd])
                   (interface: Interface[codec.Encoded],
                    resultEnc: ResultEncoder[Cmd, codec.Writer],
                    logger   : LoggerJs,
                    onError  : OnError)
                   (implicit cmdReader: codec.Reader[Cmd[_]]): Callback = {

    import codec.Encoded

    def respondForSome[A](id: Int, cmd: Cmd[A]): AsyncCallback[Unit] =
      service(cmd).attemptTry.flatMap {
        case Success(a) =>
          logger(_.info(s"Responding to request #$id with result: ${JSON.stringify(""+a).take(300)}"))
          val e = codec.encode(a)(resultEnc(cmd))
          val m = new Message(id, e)
          interface.post(m).asAsyncCallback
        case Failure(err) =>
          logger(_.error(s"Failed to service request #$id."))
          LoggerJs.exception(err)
          AsyncCallback.unit
      }

    def respond(m: Message[Encoded]): Callback = {
      val cmd = codec.decode[Cmd[_]](m.cmd)
      logger(_.info(s"Received request #${m.id}: ${JSON.stringify("" + cmd).take(300)}"))
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