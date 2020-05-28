package shipreq.webapp.client.ww.api

import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import japgolly.univeq._
import org.scalajs.dom.window.navigator
import scalajs.LinkingInfo.productionMode
import scala.util.Try
import shipreq.base.util.ErrorMsg

trait Client[Cmd[_], R[_]] {
  def post[A](cmd: Cmd[A])(implicit readResult: R[A]): AsyncCallback[A]
}

object Client {
  import org.scalajs.dom.webworkers.Worker
  import Protocol._
  import Codec.default.{Reader, Writer}

  def default[Cmd[_]](worker: Worker)(implicit writeCmd: Writer[Cmd[_]]): Client[Cmd, Reader] = {
    import Codec.{default => codec}
    apply(codec)(interface(codec, worker, _), OnError.logToConsole)
  }

  // ===================================================================================================================

  def apply[Cmd[_]](codec      : Codec)
                   (mkInterface: Callback => Interface[codec.Encoded],
                    onError    : OnError)
                   (implicit writeCmd: codec.Writer[Cmd[_]]): Client[Cmd, codec.Reader] = {

    import codec.{Encoded, Reader}

    var lastPromiseId = 0
    var promises = List.empty[Promise[Encoded]]

    val (awaitInit, onInit) = AsyncCallback.promise[Unit].runNow()

    val interface = mkInterface(onInit(Try(())))

    new Client[Cmd, codec.Reader] {

      interface.listen(receive, onError).runNow()

      override def post[A](cmd: Cmd[A])(implicit readResult: Reader[A]): AsyncCallback[A] =
        awaitInit >> AsyncCallback.promise[A].map { case (result, complete) =>
          lastPromiseId += 1
          val id = lastPromiseId
          val p = Promise[Encoded](id, msg => complete(Try(codec.decode[A](msg))))
          promises ::= p
          val msg = new Message(id, codec.encode[Cmd[_]](cmd))
          interface.post(msg).runNow()
          result
        }.asAsyncCallback.flatten.memo()

      private def popPromise(id: Int): CallbackTo[Option[Promise[Encoded]]] =
        CallbackTo {
          var result = Option.empty[Promise[Encoded]]
          promises = promises.filter { p =>
            if (p.id ==* id) {
              result = Some(p)
              false // don't keep
            } else
              true // keep
          }
          result
        }

      private def receive(m: Message[Encoded]): Callback =
        popPromise(m.id).flatMap {
          case Some(p) => p.complete(m.cmd)
          case None    => onError.handle(ErrorMsg(s"Promise #${m.id} not found"))
        }
    }
  }

  private final case class Promise[A](id: Int, complete: A => Callback)

  // ===================================================================================================================

  def interface(codec: Codec, worker: Worker, onInit: Callback): Interface[codec.Encoded] =
    new Interface[codec.Encoded] {
      import codec.Encoded

      override def listen(f: Message[Encoded] => Callback, onError: OnError): Callback =
        Callback {
          if (
            productionMode // Put first for that the Phantom check is DCE'd
              || !navigator.userAgent.contains("Phantom") // Abort if PhantomJs cos it doesn't support onerror
          ) {
            worker.onerror = Interface.onErrorFn(onError.handle)
          }
          worker.onmessage = Interface.onMessageFn(onInit, f)
        }

      override def post(msg: Message[Encoded]): Callback =
        Callback(worker.postMessage(msg, codec.transferables(msg.cmd)))
    }
}
