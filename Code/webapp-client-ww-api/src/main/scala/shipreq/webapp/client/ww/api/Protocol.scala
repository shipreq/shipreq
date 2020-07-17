package shipreq.webapp.client.ww.api

import japgolly.scalajs.react.Callback
import org.scalajs.dom.Transferable
import scala.scalajs.js
import shipreq.base.util.ErrorMsg

object Protocol {

  final class Message[+C](val id: Int, val cmd: C) extends js.Object

  // Msg sent from server to client to declare that initialisation is complete
  val Ready = "."

  // ===================================================================================================================

  sealed trait Codec {
    type Encoded
    type Reader[A]
    type Writer[A]
    def encode[A: Writer](input: A): Encoded
    def decode[A: Reader](encoded: Encoded): A
    def transferables(e: Encoded): js.UndefOr[js.Array[Transferable]]
  }

  object Codec {
    import scala.scalajs.js.typedarray._
    import scala.scalajs.js.typedarray.TypedArrayBufferOps._
    import boopickle.{PickleImpl, UnpickleImpl, Pickler}

    object Binary extends Codec {
      override type Encoded   = ArrayBuffer
      override type Reader[A] = Pickler[A]
      override type Writer[A] = Pickler[A]

      override def encode[A: Pickler](input: A): ArrayBuffer = {
        val bb = PickleImpl.intoBytes(input)
        bb.typedArray().subarray(0, bb.limit).buffer
      }

      override def decode[A: Pickler](encoded: ArrayBuffer): A = {
        val bb = TypedArrayBuffer wrap encoded
        UnpickleImpl[A].fromBytes(bb)
      }

      // https://developers.google.com/web/updates/2011/12/Transferable-Objects-Lightning-Fast
      override def transferables(e: ArrayBuffer): js.UndefOr[js.Array[Transferable]] =
        js.Array(e: Transferable)
    }

    val default: Binary.type = Binary
  }

  // ===================================================================================================================

  final case class OnError(handle: ErrorMsg => Callback) extends AnyVal {
    @inline def apply(e: ErrorMsg) = handle(e)
  }

  object OnError {
    def logToConsole: OnError =
      OnError(err => Callback(console.error(err)))
  }

  // ===================================================================================================================

  trait Interface[M] {
    def listen(h: Message[M] => Callback, e: OnError): Callback
    def post(msg: Message[M]): Callback
  }

  object Interface {
    import org.scalajs.dom.{ErrorEvent, MessageEvent}

    def onErrorFn[M](handle: ErrorMsg => Callback): js.Function1[ErrorEvent, Unit] =
      (e: ErrorEvent) => handle(ErrorMsg(e.message)).runNow()

    def onMessageFn[M](handle: Message[M] => Callback): js.Function1[MessageEvent, Unit] =
      (e: MessageEvent) => handle(e.data.asInstanceOf[Message[M]]).runNow()

    def onMessageFn[M](onInit: Callback, handle: Message[M] => Callback): js.Function1[MessageEvent, Unit] =
      (e: MessageEvent) =>
        if (Ready == e.data)
          onInit.runNow()
        else
          handle(e.data.asInstanceOf[Message[M]]).runNow()
  }
}
