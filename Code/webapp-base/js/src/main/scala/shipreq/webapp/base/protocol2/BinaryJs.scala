package shipreq.webapp.base.protocol2

import boopickle.{PickleImpl, Pickler, UnpickleImpl}
import java.nio.ByteBuffer
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array, TypedArrayBuffer}
import scala.scalajs.js.typedarray.TypedArrayBufferOps._

object BinaryJs {

  def encodeP(p: Protocol.AndValue[Pickler]): Int8Array =
    encode(p.value)(p.codec)

  def encodetoByteBufferP(p: Protocol.AndValue[Pickler]): ByteBuffer =
    PickleImpl.intoBytes(p.value)(implicitly, p.codec)

  def encode[A: Pickler](a: A): Int8Array = {
    val bb = PickleImpl.intoBytes(a)
    bb.typedArray().subarray(0, bb.limit)
  }

  def decode[A: Pickler](ab: ArrayBuffer): A = {
    val bb = TypedArrayBuffer.wrap(ab)
    UnpickleImpl[A].fromBytes(bb)
  }

  def decodeUnsafe[A: Pickler](a: Any): A =
    decode[A](a.asInstanceOf[ArrayBuffer])

}
