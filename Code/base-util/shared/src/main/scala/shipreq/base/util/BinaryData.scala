package shipreq.base.util

import japgolly.univeq.UnivEq
import java.io.OutputStream
import java.nio.ByteBuffer
import java.util.Arrays

/** Immutable blob of binary data. */
final class BinaryData(private[BinaryData] val bytes: Array[Byte], val length: Int) {

  // Note: It's acceptable to have excess bytes beyond the declared length
  assert(length <= bytes.length, s"length ($length) exceeds number of bytes (${bytes.length})")

  override def toString = s"BinaryData(${describe()})"

  override def hashCode =
    // Should use Arrays.hashCode() but have to copy to use provided length instead of array.length
    length

  override def equals(o: Any): Boolean =
    o match {
      case b: BinaryData => (length == b.length) && (0 until length).forall(i => bytes(i) == b.bytes(i))
      case _             => false
    }

  def duplicate: BinaryData =
    BinaryData.unsafeFromArray(toNewArray)

  def describe(byteLimit: Int = BinaryData.DefaultByteLimitInDesc) = {
    val byteDesc = describeBytes(byteLimit)
    val len = "%,d".format(length)
    s"$len bytes: $byteDesc"
  }

  def describeBytes(limit: Int = BinaryData.DefaultByteLimitInDesc) = {
    var i = bytes.iterator.map(b => "%02X".format(b & 0xff))
    if (length > limit)
      i = i.take(limit) ++ Iterator.single("…")
    else
      i = i.take(length)
    i.mkString(",")
  }

  def writeTo(os: OutputStream): Unit =
    os.write(bytes, 0, length)

  def toByteBuffer: ByteBuffer =
    unsafeByteBuffer.asReadOnlyBuffer()

  def toNewArray: Array[Byte] =
    Arrays.copyOf(bytes, length)

  /** unsafe in that you might get back the underlying array which is mutable */
  def unsafeArray: Array[Byte] =
    if (length == bytes.length)
      bytes
    else
      bytes.take(length)

  /** unsafe in that the result is mutable */
  def unsafeByteBuffer: ByteBuffer =
    ByteBuffer.wrap(bytes, 0, length)
}

object BinaryData {

  implicit def univEq: UnivEq[BinaryData] = UnivEq.force

  val DefaultByteLimitInDesc = 50

//  val empty: BinaryData =
//    unsafeFromArray(new Array(0))

  def fromArray(a: Array[Byte]): BinaryData = {
    val a2 = Arrays.copyOf(a, a.length)
    unsafeFromArray(a2)
  }

  def fromByteBuffer(bb: ByteBuffer): BinaryData =
    if (bb.hasArray) {
      val a = Arrays.copyOf(bb.array(), bb.limit())
      unsafeFromArray(a)
    } else {
      val a = new Array[Byte](bb.remaining)
      bb.get(a)
      unsafeFromArray(a)
    }

  /** unsafe because the array could be modified later and affect the underlying array we use here */
  def unsafeFromArray(a: Array[Byte]): BinaryData =
    new BinaryData(a, a.length)

  /** unsafe because the ByteBuffer could be modified later and affect the underlying array we use here */
  def unsafeFromByteBuffer(bb: ByteBuffer): BinaryData =
    if (bb.hasArray)
      new BinaryData(bb.array(), bb.limit())
    else
      fromByteBuffer(bb)
}