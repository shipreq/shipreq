package shipreq.webapp.client.ww.api

import japgolly.scalajs.react.AsyncCallback
import org.scalajs.dom.window.navigator
import scalajs.LinkingInfo.productionMode
import scala.util.Try
import scala.collection.immutable.ListMap

trait Client[Cmd[_], R[_]] {
  def post[A](cmd: Cmd[A])(implicit readResult: R[A]): AsyncCallback[A]
}

object Client extends Settings {
  import org.scalajs.dom.ErrorEvent
  import org.scalajs.dom.webworkers.Worker
  import Protocol._
  import codec._

  def apply[Cmd[_]](worker: Worker)(implicit writeCmd: Writer[Cmd[_]]): Client[Cmd, Reader] =
    new Impl(codec, new InterfaceImpl(worker), onError)

  private final class InterfaceImpl(worker: Worker) extends Interface[Encoded] {
    override def listen(hnd: Message[Encoded] => Unit, onError: OnError): Unit = {
      if (
        productionMode // Put first for that the Phantom check is DCE'd
          || !navigator.userAgent.contains("Phantom") // Abort if PhantomJs cos it doesn't support onerror
      ) {
        worker.onerror = (e: ErrorEvent) => onError(e.message)
      }
      worker.onmessage = Interface.onMessageFn(hnd)
    }

    override def post(msg: Message[Encoded]): Unit =
      worker.postMessage(msg, transferables(msg.cmd))
  }

  private final class Impl[Cmd[_], Enc, R[_], W[_]](codec    : Codec[Enc, R, W],
                                                    interface: Interface[Enc],
                                                    onError  : OnError)
                                                   (implicit writeCmd: W[Cmd[_]]) extends Client[Cmd, R] {

    private var i = 0
    private var callbacks = ListMap.empty[Int, Enc => Unit]

    interface.listen(receiveResult, onError)

    override def post[A](cmd: Cmd[A])(implicit readResult: R[A]): AsyncCallback[A] =
      AsyncCallback.promise[A].map { case (result, complete) =>
        i += 1
        callbacks = callbacks.updated(i, e => complete(Try(codec.decode[A](e))).runNow())
        interface.post(new Message(i, codec.encode[Cmd[_]](cmd)))
        result
      }.asAsyncCallback.flatten.memo()

    private def receiveResult(m: Message[Enc]): Unit =
      callbacks.get(m.key) match {
        case Some(cb) => callbacks -= m.key; cb(m.cmd)
        case None     => onError(s"Callback not found for Message(${m.key}, ${m.cmd}).")
      }
  }
}
