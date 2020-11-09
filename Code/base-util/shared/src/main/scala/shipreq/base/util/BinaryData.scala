package shipreq.base.util

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

  def describe(byteLimit: Int = BinaryData.DefaultByteLimitInDesc, sep: String = ",") = {
    val byteDesc = describeBytes(byteLimit, sep)
    val len = "%,d".format(length)
    s"$len bytes: $byteDesc"
  }

  def describeBytes(limit: Int = BinaryData.DefaultByteLimitInDesc, sep: String = ",") = {
    var i = bytes.iterator.map(b => "%02X".format(b & 0xff))
    if (length > limit)
      i = i.take(limit) ++ Iterator.single("…")
    else
      i = i.take(length)
    i.mkString(sep)
  }

  def writeTo(os: OutputStream): Unit =
    os.write(bytes, 0, length)

  def toByteBuffer: ByteBuffer =
    unsafeByteBuffer.asReadOnlyBuffer()

  def toNewByteBuffer: ByteBuffer =
    ByteBuffer.wrap(toNewArray, 0, length)

  def toNewArray: Array[Byte] =
    Arrays.copyOf(bytes, length)

  /** unsafe in that you might get back the underlying array which is mutable */
  def unsafeArray: Array[Byte] =
    if (length == bytes.length)
      bytes
    else
      bytes.take(length)

  def binaryLikeString: String = {
    val chars = new Array[Char](length)
    var j = length
    while (j > 0) {
      j -= 1
      val b = bytes(j)
      val i = b.toInt & 0xff
      chars.update(j, i.toChar)
    }
    String.valueOf(chars)
  }

  /** unsafe in that the result is mutable */
  def unsafeByteBuffer: ByteBuffer =
    ByteBuffer.wrap(bytes, 0, length)

  def hex: String =
    bytes
      .iterator
      .take(length)
      .map(b => "%02X".format(b & 0xff))
      .mkString

  def ++(that: BinaryData): BinaryData =
    BinaryData.unsafeFromArray(this.unsafeArray ++ that.unsafeArray)
}

object BinaryData {

  implicit def univEq: UnivEq[BinaryData] = UnivEq.force

  val DefaultByteLimitInDesc = 50

  def empty: BinaryData =
    unsafeFromArray(new Array(0))

  def fromArray(a: Array[Byte]): BinaryData = {
    val a2 = Arrays.copyOf(a, a.length)
    unsafeFromArray(a2)
  }

  def fromArraySeq(a: ArraySeq[Byte]): BinaryData =
    unsafeFromArray(a.unsafeArray.asInstanceOf[Array[Byte]])

  def fromByteBuffer(bb: ByteBuffer): BinaryData =
    if (bb.hasArray) {
      val a = Arrays.copyOf(bb.array(), bb.limit())
      unsafeFromArray(a)
    } else {
      val a = new Array[Byte](bb.remaining)
      bb.get(a)
      unsafeFromArray(a)
    }

  def fromHex(hex: String): BinaryData = {
    assert((hex.length & 1) == 0, "Hex strings must have an even length.")
    var i = hex.length >> 1
    val bytes = new Array[Byte](i)
    while (i > 0) {
      i -= 1
      val si = i << 1
      val byteStr = hex.substring(si, si + 2)
      val byte = java.lang.Integer.parseUnsignedInt(byteStr, 16).byteValue()
      bytes(i) = byte
    }
    unsafeFromArray(bytes)
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