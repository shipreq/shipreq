package shipreq.webapp.base.protocol2

import boopickle.{PickleImpl, Pickler, UnpickleImpl}
import java.nio.ByteBuffer
import org.scalajs.dom.window
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array, TypedArrayBuffer}
import scala.scalajs.js.typedarray.TypedArrayBufferOps._

object BinaryJs {

  def encodeToArrayBuffer[A: Pickler](a: A): ArrayBuffer =
    byteBufferToArrayBuffer(PickleImpl.intoBytes(a))

  // ===================================================================================================================

  def decodeFromArrayBuffer[A: Pickler](ab: ArrayBuffer): A = {
    val bb = TypedArrayBuffer.wrap(ab)
    UnpickleImpl[A].fromBytes(bb)
  }

  def decodeFromArrayBufferUnsafe[A: Pickler](a: Any): A =
    decodeFromArrayBuffer[A](a.asInstanceOf[ArrayBuffer])

  // ===================================================================================================================

  def byteBufferToArrayBuffer(bb: ByteBuffer): ArrayBuffer =
    // TODO hmmm? ByteBuffer -> Int8Array -> ArrayBuffer
    int8ArrayToArrayBuffer(byteBufferToInt8Array(bb))

  def byteBufferToInt8Array(bb: ByteBuffer): Int8Array =
    bb.typedArray().subarray(0, bb.limit)

  def int8ArrayToArrayBuffer(a: Int8Array): ArrayBuffer =
    a.buffer.slice(0, a.length)

  def base64ToByteBuffer(base64: String): ByteBuffer = {
    val binstr = window.atob(base64)
    val buf = new Int8Array(binstr.length)
    var i = 0
    binstr.foreach { ch =>
      buf(i) = ch.toByte
      i += 1
    }
    TypedArrayBuffer.wrap(buf)
  }

}
