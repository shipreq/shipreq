package shipreq.webapp.member.protocol.webworker

import japgolly.scalajs.react._
import org.scalajs.dom.{AbstractWorker, DedicatedWorkerGlobalScope, ErrorEvent, ExtendableMessageEvent, MessageEvent, MessagePort, SharedWorker, SharedWorkerGlobalScope, Transferable, Worker, WorkerGlobalScope}
import scala.scalajs.js
import scala.scalajs.js.{isUndefined, |}
import shipreq.base.util.ErrorMsg

object AbstractWebWorker {

  type TransferList = js.UndefOr[js.Array[Transferable]]

  trait Client {
    def onError(f: OnError): Callback
    def listen(f: js.Any => Callback): Callback
    def send(msg: js.Any, transferList: TransferList): Callback
  }

  object Client {

    def apply(url: String, name: String): CallbackTo[Client] =
      CallbackTo {
        if (js.isUndefined(js.Dynamic.global.SharedWorker))
          dedicated(new Worker(url))
        else
          shared(new SharedWorker(url, name))
      }

    def dedicated(worker: Worker): Client =
      this.worker(worker, worker.asInstanceOf[js.Dynamic])

    def shared(worker: SharedWorker): Client =
      this.worker(worker, worker.port.asInstanceOf[js.Dynamic])

    private def worker(worker: AbstractWorker, port: js.Dynamic): Client =
      new Client {

        override def onError(f: OnError): Callback =
          setErrorHandler(worker, f)

        override def listen(f: js.Any => Callback): Callback =
          Callback {
            val onmessage: js.Function1[MessageEvent, _] = { e =>
              f(e.data.asInstanceOf[js.Any]).runNow()
            }
            port.onmessage = onmessage
          }

        override def send(msg: js.Any, transferList: TransferList): Callback =
          Callback {
            port.postMessage(msg, transferList)
          }
      }
  }

  // ===================================================================================================================

  trait Server {
    type Client
    def onError(f: OnError): Callback
    def listen(f: Client => CallbackTo[js.Any => Callback]): Callback
    def send(to: IterableOnce[Client], msg: js.Any, transferList: TransferList): Callback
  }

  object Server {

    def apply(): Server = {
      val self = js.Dynamic.global.self
      if (isUndefined(self.name))
        dedicated(self.asInstanceOf[DedicatedWorkerGlobalScope])
      else
        shared(self.asInstanceOf[SharedWorkerGlobalScope])
    }

    def dedicated(worker: DedicatedWorkerGlobalScope): Server =
      new Server {
        override type Client = Unit

        override def onError(f: OnError): Callback =
          setErrorHandler(worker, f)

        override def listen(f: Client => CallbackTo[js.Any => Callback]): Callback =
          Callback {
            val onMsg = f(()).runNow()
            worker.onmessage = { e =>
              onMsg(e.data.asInstanceOf[js.Any]).runNow()
            }
          }

        override def send(to: IterableOnce[Client], msg: js.Any, transferList: TransferList): Callback =
          Callback {
            if (to.iterator.nonEmpty) {
              worker.postMessage(msg, transferList)
            }
          }
      }

    def shared(worker: SharedWorkerGlobalScope): Server =
      new Server {
        override type Client = MessagePort

        override def onError(f: OnError): Callback =
          setErrorHandler(worker, f)

        override def listen(f: Client => CallbackTo[js.Any => Callback]): Callback =
          Callback {
            val onconnect: js.Function1[ExtendableMessageEvent, _] = { e =>
              for (port <- e.ports) {
                val onMsg = f(port).runNow()
                port.onmessage = { e =>
                  onMsg(e.data.asInstanceOf[js.Any]).runNow()
                }
              }
            }
            worker.asInstanceOf[js.Dynamic].onconnect = onconnect // https://github.com/scala-js/scala-js-dom/issues/427
          }

        override def send(to: IterableOnce[Client], msg: js.Any, transferList: TransferList): Callback =
          Callback {
            val a = msg.asInstanceOf[js.Any]
            val it = to.iterator
            while (it.hasNext) {
              val port = it.next()
              val isLast = it.isEmpty
              if (isLast)
                port.postMessage(a, transferList)
              else
                port.postMessage(a)
            }
          }
      }

  }

  // ===================================================================================================================

  private def setErrorHandler(worker: AbstractWorker | WorkerGlobalScope, f: OnError): Callback =
    Callback {
      val onerror: js.Function1[ErrorEvent, _] = { e =>
        val errMsg = ErrorMsg(e.message)
        f(errMsg).runNow()
      }
      worker.asInstanceOf[js.Dynamic].onerror = onerror
    }

}
