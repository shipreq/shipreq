package shipreq.webapp.member.protocol.webworker

import org.scalajs.dom.Transferable
import scala.scalajs.js

sealed trait WebWorkerProtocol {
  type Encoded
  type Reader[A]
  type Writer[A]
  def encode[A: Writer](input: A): Encoded
  def decode[A: Reader](encoded: Encoded): A
  def transferables(e: Encoded): js.UndefOr[js.Array[Transferable]]
}

object WebWorkerProtocol {
  import scala.scalajs.js.typedarray._
  import scala.scalajs.js.typedarray.TypedArrayBufferOps._
  import boopickle.{PickleImpl, UnpickleImpl, Pickler}

  object Binary extends WebWorkerProtocol {
    override type Encoded   = ArrayBuffer
    override type Reader[A] = Pickler[A]
    override type Writer[A] = Pickler[A]

    override def encode[A: Pickler](input: A): ArrayBuffer = {
      val bb  = PickleImpl.intoBytes(input)
      val len = bb.limit()
      val ia  = bb.typedArray().subarray(0, len)
      val ab  = ia.buffer.slice(0, len)
      ab
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
