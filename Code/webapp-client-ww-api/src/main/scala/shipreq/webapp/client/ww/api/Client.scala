package shipreq.webapp.client.ww.api

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.Try
import scalajs.js
import Protocol._

final class Client[Cmd <: AbstractCmd, Enc, R[_], W[_]](codec    : Codec[Enc, R, W],
                                                        interface: Interface[Enc],
                                                        onError  : OnError)
                                                       (implicit writeCmd: W[Cmd]) {

  private var i = 0
  private val callbacks = mutable.ListMap.empty[Int, Enc => Unit]

  interface.listen(receiveResult, onError)

  def post(cmd: Cmd)(implicit rr: R[cmd.Result]): Future[cmd.Result] = {
    i += 1
    val p = Promise[cmd.Result]()
    callbacks.update(i, e => p tryComplete Try(codec.decode[cmd.Result](e)))
    interface.post(new Message(i, codec.encode[Cmd](cmd)))
    p.future
  }

  private def receiveResult(m: Message[Enc]): Unit =
    callbacks.remove(m.key) match {
      case Some(cb) => cb(m.cmd)
      case None     => onError(s"Callback not found for Message(${m.key}, ${m.cmd}).")
    }
}

object Client extends Settings {
  import org.scalajs.dom.ErrorEvent
  import org.scalajs.dom.webworkers.Worker
  import codec._

  def apply[Cmd <: AbstractCmd : Writer](worker: Worker): Client[Cmd, Encoded, Reader, Writer] =
    new Client(codec, new WebWorkerInterface(worker), onError)

  final class WebWorkerInterface(worker: Worker) extends Interface[Encoded] {
    override def listen(hnd: Message[Encoded] => Unit, onError: OnError): Unit = {
      worker.onerror = (e: ErrorEvent) => onError(e.message)
      worker.onmessage = Interface.onMessageFn(hnd)
        .asInstanceOf[js.Function1[js.Any, Unit]] // TODO Remove after scala-dom-js upgrade
    }

    override def post(msg: Message[Encoded]): Unit =
      worker.postMessage(msg, transferables(msg.cmd))
  }
}

