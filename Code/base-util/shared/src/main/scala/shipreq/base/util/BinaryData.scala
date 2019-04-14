package shipreq.base.util

import japgolly.univeq.UnivEq
import java.io.OutputStream
import java.nio.ByteBuffer
import java.util.Arrays

/** Immutable blob of binary data. */
final class BinaryData(private[BinaryData] val bytes: Array[Byte], val length: Int) {

  override def toString = s"BinaryData($length bytes)"

  override def hashCode =
    // Should use Arrays.hashCode() but have to copy to use provided length instead of array.length
    length

  override def equals(o: Any): Boolean =
    o match {
      case b: BinaryData => (length == b.length) && (0 until length).forall(i => bytes(i) == b.bytes(i))
      case _             => false
    }

  def writeTo(os: OutputStream): Unit =
    os.write(bytes, 0, length)

  def toByteBuffer: ByteBuffer =
    ByteBuffer.wrap(bytes, 0, length).asReadOnlyBuffer()

  def toNewArray: Array[Byte] =
    Arrays.copyOf(bytes, length)
}

object BinaryData {

  implicit def univEq: UnivEq[BinaryData] = UnivEq.force

//  val empty: BinaryData =
//    unsafeFromArray(new Array(0))

  def fromArray(a: Array[Byte]): BinaryData = {
    val a2 = Arrays.copyOf(a, a.length)
    unsafeFromArray(a2)
  }

  /** unsafe because the array could be modified later and affect the underlying array we use here */
  def unsafeFromArray(a: Array[Byte]): BinaryData =
    new BinaryData(a, a.length)

  /** unsafe because the ByteBuffer could be modified later and affect the underlying array we use here */
  def unsafeFromByteBuffer(bb: ByteBuffer): BinaryData =
    new BinaryData(bb.array(), bb.limit())
}