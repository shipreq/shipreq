package shipreq.base.util

import java.io.OutputStream
import java.nio.ByteBuffer
import java.util.Arrays

/** Immutable blob of binary data. */
final class BinaryData(private[BinaryData] val bytes: Array[Byte],
                       private[BinaryData] val offset: Int,
                       val length: Int) {

  private val lastIndExcl = offset + length

  // Note: It's acceptable to have excess bytes beyond the declared length
  assert(lastIndExcl <= bytes.length, s"offset($offset) + length ($length) exceeds number of bytes (${bytes.length})")

  override def toString = s"BinaryData(${describe()})"

  override def hashCode =
    // Should use Arrays.hashCode() but have to copy to use provided length instead of array.length
    length

  override def equals(o: Any): Boolean =
    o match {
      case b: BinaryData =>
        @inline def sameRef = this eq b
        @inline def sameLen = length == b.length
        @inline def sameBin = (0 until length).forall(i => bytes(offset + i) == b.bytes(b.offset + i))
        sameRef || (sameLen && sameBin)
      case _ =>
        false
    }

  @inline def isEmpty: Boolean =
    length == 0

  @inline def nonEmpty: Boolean =
    length != 0

  def duplicate: BinaryData =
    BinaryData.unsafeFromArray(toNewArray)

  def describe(byteLimit: Int = BinaryData.DefaultByteLimitInDesc, sep: String = ",") = {
    val byteDesc = describeBytes(byteLimit, sep)
    val len = "%,d".format(length)
    s"$len bytes: $byteDesc"
  }

  def describeBytes(limit: Int = BinaryData.DefaultByteLimitInDesc, sep: String = ",") = {
    var i = bytes.iterator.drop(offset).map(b => "%02X".format(b & 0xff))
    if (length > limit)
      i = i.take(limit) ++ Iterator.single("…")
    else
      i = i.take(length)
    i.mkString(sep)
  }

  def writeTo(os: OutputStream): Unit =
    os.write(bytes, offset, length)

  // Note: the below must remain a `def` because ByteBuffers themselves have mutable state
  /** unsafe in that the underlying bytes could be modified via access to unsafeArray */
  def unsafeByteBuffer: ByteBuffer =
    if (offset > 0)
      ByteBuffer.wrap(bytes, 0, lastIndExcl).position(offset).slice()
    else
      ByteBuffer.wrap(bytes, 0, length)

  def toNewByteBuffer: ByteBuffer =
    ByteBuffer.wrap(toNewArray, 0, length)

  def toNewArray: Array[Byte] =
    Arrays.copyOfRange(bytes, offset, lastIndExcl)

  /** unsafe in that you might get back the underlying array which is mutable */
  lazy val unsafeArray: Array[Byte] =
    if (offset == 0 && length == bytes.length)
      bytes
    else
      toNewArray

  def binaryLikeString: String = {
    val chars = new Array[Char](length)
    var j = length
    while (j > 0) {
      j -= 1
      val b = bytes(offset + j)
      val i = b.toInt & 0xff
      chars.update(j, i.toChar)
    }
    String.valueOf(chars)
  }

  def hex: String =
    bytes
      .iterator
      .slice(offset, lastIndExcl)
      .map(b => "%02X".format(b & 0xff))
      .mkString

  def ++(that: BinaryData): BinaryData =
    BinaryData.unsafeFromArray(this.unsafeArray ++ that.unsafeArray)

  def drop(n: Int): BinaryData = {
    val m = n.min(length)
    new BinaryData(bytes, offset + m, length - m)
  }
}

object BinaryData {

  implicit def univEq: UnivEq[BinaryData] =
    UnivEq.force

  final val DefaultByteLimitInDesc = 50

  def empty: BinaryData =
    unsafeFromArray(new Array(0))

  def byte(b: Byte): BinaryData = {
    val a = new Array[Byte](1)
    a(0) = b
    unsafeFromArray(a)
  }

  def fromArray(a: Array[Byte]): BinaryData = {
    val a2 = Arrays.copyOf(a, a.length)
    unsafeFromArray(a2)
  }

  def fromArraySeq(a: ArraySeq[Byte]): BinaryData =
    unsafeFromArray(a.unsafeArray.asInstanceOf[Array[Byte]])

  def fromByteBuffer(bb: ByteBuffer): BinaryData =
    if (bb.hasArray) {
      val offset = bb.arrayOffset()
      val a = Arrays.copyOfRange(bb.array(), offset, offset + bb.limit())
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
    new BinaryData(a, 0, a.length)

  /** unsafe because the ByteBuffer could be modified later and affect the underlying array we use here */
  def unsafeFromByteBuffer(bb: ByteBuffer): BinaryData =
    if (bb.hasArray)
      new BinaryData(bb.array(), bb.arrayOffset(), bb.limit())
    else
      fromByteBuffer(bb)
}