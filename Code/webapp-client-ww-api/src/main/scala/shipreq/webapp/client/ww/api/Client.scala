package shipreq.webapp.client.ww.api

import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo, Reusability}
import org.scalajs.dom.window.navigator
import scala.scalajs.LinkingInfo.productionMode
import scala.util.Try
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.lib.LoggerJs

trait Client[Cmd[_], R[_], Enc] {
  final def post[A](cmd: Cmd[A])(implicit readResult: R[A]): AsyncCallback[A] =
    postEnc(cmd, encode(cmd))

  def encode(cmd: Cmd[_]): Enc

  def postEnc[A](cmd: Cmd[A], enc: Enc)(implicit readResult: R[A]): AsyncCallback[A]
}

object Client {
  import org.scalajs.dom.Worker
  import Protocol._
  import Codec.{default => D}

  def default[Cmd[_]](worker: Worker, logger: LoggerJs)(implicit writeCmd: D.Writer[Cmd[_]]): Client[Cmd, D.Reader, D.Encoded] = {
    import Codec.{default => codec}
    apply(codec)(interface(codec, worker, _), logger, OnError.logToConsole)
  }

  implicit def reusability[Cmd[_], R[_], E]: Reusability[Client[Cmd, R, E]] =
    Reusability.byRef

  // ===================================================================================================================

  def apply[Cmd[_]](codec      : Codec)
                   (mkInterface: Callback => Interface[codec.Encoded],
                    logger     : LoggerJs,
                    onError    : OnError)
                   (implicit writeCmd: codec.Writer[Cmd[_]]): Client[Cmd, codec.Reader, codec.Encoded] = {

    import codec.{Encoded, Reader}

    var lastPromiseId = 0
    var promises = List.empty[Promise[Encoded]]

    val initBarrier = AsyncCallback.barrier.runNow()

    val interface = mkInterface(initBarrier.complete)

    new Client[Cmd, Reader, Encoded] {

      interface.listen(receive, onError).runNow()

      override def encode(cmd: Cmd[_]) =
        codec.encode[Cmd[_]](cmd)

      override def postEnc[A](cmd: Cmd[A], enc: Encoded)(implicit readResult: Reader[A]): AsyncCallback[A] =
        initBarrier.await >> AsyncCallback.promise[A].map { case (result, complete) =>
          lastPromiseId += 1
          val id = lastPromiseId
          def listener(msg: Encoded): Callback = {
            val decoded = Try(codec.decode[A](msg))
            logger(_.info(s"Received WW response #$id: ${decoded.fold(_.toString, a => JSON.stringify("" + a).take(300))}"))
            complete(decoded)
          }
          val p = Promise[Encoded](id, listener)
          promises ::= p
          val msg = new Message(id, enc)
          logger(_.info(s"Sending WW request #$id: ${JSON.stringify("" + cmd).take(300)}"))
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
