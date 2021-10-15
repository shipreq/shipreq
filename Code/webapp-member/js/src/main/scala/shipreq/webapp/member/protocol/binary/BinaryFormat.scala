package shipreq.webapp.member.protocol.binary

import boopickle.{PickleImpl, Pickler, UnpickleImpl}
import japgolly.scalajs.react.AsyncCallback
import java.nio.ByteBuffer
import scala.scalajs.js
import shipreq.base.util.BinaryData
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.v1.BaseData.unsupportedVer

final class BinaryFormat[A](val encode: A => AsyncCallback[BinaryData],
                            val decode: BinaryData => AsyncCallback[A]) {

  def xmap[B](onDecode: A => B)(onEncode: B => A): BinaryFormat[B] =
  // Delegating because decoding can fail and must be wrapped to be pure
    xmapAsync(
      a => AsyncCallback.delay(onDecode(a)))(
      b => AsyncCallback.delay(onEncode(b)))

  def xmapAsync[B](onDecode: A => AsyncCallback[B])(onEncode: B => AsyncCallback[A]): BinaryFormat[B] =
    BinaryFormat.async(
      decode(_).flatMap(onDecode))(
      onEncode(_).flatMap(encode))

  type ThisIsBinary = BinaryFormat[A] =:= BinaryFormat[BinaryData]

  def encrypt(e: Encryption)(implicit ev: ThisIsBinary): BinaryFormat[BinaryData] =
    ev(this).xmapAsync(e.decrypt)(e.encrypt)

  def compress(c: Compression)(implicit ev: ThisIsBinary): BinaryFormat[BinaryData] =
    ev(this).xmap(c.decompressOrThrow)(c.compress)

  def pickle[B](implicit pickler: SafePickler[B], ev: ThisIsBinary): BinaryFormat[B] =
    ev(this).xmap(pickler.decodeOrThrow)(pickler.encode)

  def pickleBasic[B](implicit pickler: Pickler[B], ev: ThisIsBinary): BinaryFormat[B] = {
    val unpickle = UnpickleImpl[B]
    ev(this)
      .xmap[ByteBuffer](_.unsafeByteBuffer)(BinaryData.unsafeFromByteBuffer)
      .xmap(unpickle.fromBytes(_))(PickleImpl.intoBytes(_))
  }
}

object BinaryFormat {

  val id: BinaryFormat[BinaryData] = {
    val f: BinaryData => AsyncCallback[BinaryData] =
      AsyncCallback.pure
    async(f)(f)
  }

  def apply[A](decode: BinaryData => A)
              (encode: A => BinaryData): BinaryFormat[A] =
    async(
      b => AsyncCallback.delay(decode(b)))(
      a => AsyncCallback.delay(encode(a)))

  def async[A](decode: BinaryData => AsyncCallback[A])
              (encode: A => AsyncCallback[BinaryData]): BinaryFormat[A] =
    new BinaryFormat(encode, decode)

  def versioned[A](oldest: BinaryFormat[A], latestLast: BinaryFormat[A]*): BinaryFormat[A] = {
    val layers          = oldest +: latestLast.toArray
    val decoders        = layers
    val decoderIndices  = decoders.indices
    val latestVer       = decoders.length - 1
    val latestVerHeader = BinaryData.byte(latestVer.toByte)
    val encoder         = layers.last

    def encode(a: A): AsyncCallback[BinaryData] =
      encoder.encode(a).map(latestVerHeader ++ _)

    def decode(bin: BinaryData): AsyncCallback[A] =
      AsyncCallback.suspend {

        if (bin.isEmpty)
          throw js.JavaScriptException("No data")

        val ver = bin.unsafeArray(0).toInt

        if (decoderIndices.contains(ver)) {
          val binBody = bin.drop(1)
          decoders(ver).decode(binBody)
        } else if (ver < 0)
          throw js.JavaScriptException("Bad data")
        else
          unsupportedVer(ver, latestVer)
      }

    async(decode)(encode)
  }

  def pickleCompressEncrypt[A](c: Compression, e: Encryption)(implicit pickler: SafePickler[A]): BinaryFormat[A] =
    id
      .encrypt(e) // Encryption is the very last step
      .compress(c) // Here we compress the binary *before* encrypting
      .pickle[A]
}
