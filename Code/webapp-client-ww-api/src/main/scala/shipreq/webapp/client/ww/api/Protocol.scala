package shipreq.webapp.client.ww.api

import scalajs.js
import org.scalajs.dom.console

object Protocol {

  final class Message[+C](val key: Int, val cmd: C) extends js.Object

  sealed trait Codec[E, R[_], W[_]] {
    final type Encoded   = E
    final type Reader[A] = R[A]
    final type Writer[A] = W[A]
    def encode[A: Writer](input: A): Encoded
    def decode[A: Reader](encoded: Encoded): A
  }

  object Codec {
    import scala.scalajs.js.typedarray._
    import scala.scalajs.js.typedarray.TypedArrayBufferOps._
    import boopickle.{PickleImpl, UnpickleImpl, Pickler}

    object Binary extends Codec[ArrayBuffer, Pickler, Pickler] {
      override def encode[A: Pickler](input: A): ArrayBuffer = {
        val bb = PickleImpl.intoBytes(input)
        bb.typedArray().subarray(0, bb.limit).buffer
      }

      override def decode[A: Pickler](encoded: ArrayBuffer): A = {
        val bb = TypedArrayBuffer wrap encoded
        UnpickleImpl[A].fromBytes(bb)
      }
    }
  }

  final case class OnError(handle: String => Unit) extends AnyVal {
    @inline def apply(s: String) = handle(s)
  }

  object OnError {
    def Console: OnError =
      OnError(console.error(_))
  }

  trait Interface[M] {
    def listen(h: Message[M] => Unit, e: OnError): Unit
    def post(msg: Message[M]): Unit
  }

  object Interface {
    import org.scalajs.dom.MessageEvent

    def onMessageFn[M](handle: Message[M] => Unit): js.Function1[MessageEvent, Unit] =
      (e: MessageEvent) => handle(e.data.asInstanceOf[Message[M]])
  }
}
