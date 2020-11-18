package shipreq.webapp.member.protocol.indexeddb

import boopickle.{PickleImpl, Pickler, UnpickleImpl}
import japgolly.scalajs.react.{AsyncCallback, CallbackTo}
import java.nio.ByteBuffer
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import shipreq.base.util.BinaryData
import shipreq.base.util.JsExt._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.v1.BaseData.unsupportedVer
import shipreq.webapp.member.protocol.binary.{Compression, Encryption}

final case class ValueCodec[A](encode: A => CallbackTo[js.Any],
                               decode: js.Any => CallbackTo[A]) {

  def xmap[B](onDecode: A => B)(onEncode: B => A): ValueCodec[B] =
    // Delegating because decoding can fail and must be wrapped to be pure
    xmapSync(
      a => CallbackTo(onDecode(a)))(
      b => CallbackTo(onEncode(b)))

  def xmapSync[B](onDecode: A => CallbackTo[B])(onEncode: B => CallbackTo[A]): ValueCodec[B] =
    ValueCodec[B](
      encode = onEncode(_).flatMap(encode),
      decode = decode(_).flatMap(onDecode))

  def async: ValueCodec.Async[A] =
    ValueCodec.Async(
      encode = encode.andThen(_.asAsyncCallback),
      decode = decode.andThen(_.asAsyncCallback))

  type ThisIsBinary = ValueCodec[A] =:= ValueCodec[BinaryData]

  def compress(c: Compression)(implicit ev: ThisIsBinary): ValueCodec[BinaryData] =
    ev(this).xmap(c.decompressOrThrow)(c.compress)

  def pickle[B](implicit pickler: SafePickler[B], ev: ThisIsBinary): ValueCodec[B] =
    ev(this).xmap(pickler.decodeOrThrow)(pickler.encode)
}

object ValueCodec {

  val binary: ValueCodec[BinaryData] =
    apply(
      encode = b => CallbackTo.pure(b.unsafeArrayBuffer),
      decode = d => CallbackTo(BinaryData.unsafeFromArrayBuffer(d.asInstanceOf[ArrayBuffer]))
    )

  lazy val string: ValueCodec[String] =
    apply(
      encode = s => CallbackTo.pure(s),
      decode = d => CallbackTo(
        (d: Any) match {
          case s: String => s
          case x         => throw new RuntimeException("String expected: " + x)
        }
      )
    )

  // ===================================================================================================================

  final case class Async[A](encode: A => AsyncCallback[js.Any],
                            decode: js.Any => AsyncCallback[A]) {

    def xmap[B](onDecode: A => B)(onEncode: B => A): Async[B] =
      // Delegating because decoding can fail and must be wrapped to be pure
      xmapAsync(
        a => AsyncCallback.delay(onDecode(a)))(
        b => AsyncCallback.delay(onEncode(b)))

    def xmapAsync[B](onDecode: A => AsyncCallback[B])(onEncode: B => AsyncCallback[A]): Async[B] =
      Async[B](
        encode = onEncode(_).flatMap(encode),
        decode = decode(_).flatMap(onDecode))

    type ThisIsBinary = Async[A] =:= Async[BinaryData]

    def encrypt(e: Encryption)(implicit ev: ThisIsBinary): Async[BinaryData] =
      ev(this).xmapAsync(e.decrypt)(e.encrypt)

    def compress(c: Compression)(implicit ev: ThisIsBinary): Async[BinaryData] =
      ev(this).xmap(c.decompressOrThrow)(c.compress)

    def pickle[B](implicit pickler: SafePickler[B], ev: ThisIsBinary): Async[B] =
      ev(this).xmap(pickler.decodeOrThrow)(pickler.encode)

    def pickleBasic[B](implicit pickler: Pickler[B], ev: ThisIsBinary): Async[B] = {
      val unpickle = UnpickleImpl[B]
      ev(this)
        .xmap[ByteBuffer](_.unsafeByteBuffer)(BinaryData.unsafeFromByteBuffer)
        .xmap(unpickle.fromBytes(_))(PickleImpl.intoBytes(_))
    }
  }

  object Async {

    val binary = ValueCodec.binary.async

    type BinaryLayer[A] = Async[BinaryData] => Async[A]

    def versionedBinary[A](layersOldestFirst: BinaryLayer[A]*): Async[A] = {
      assert(layersOldestFirst.nonEmpty)
      val layers          = layersOldestFirst.toArray
      val decoders        = layers.map(_(binary))
      val decoderIndices  = decoders.indices
      val latestVer       = decoders.length - 1
      val latestVerHeader = BinaryData.byte(latestVer.toByte)
      val encoder         = layers.last(ValueCodec.binary.xmap[BinaryData](_ => null)(latestVerHeader ++ _).async)

      def decode(bin: BinaryData): AsyncCallback[A] =
        AsyncCallback.byName {

          if (bin.isEmpty)
            throw js.JavaScriptException("No data")

          val ver = bin.unsafeArray(0).toInt

          if (decoderIndices.contains(ver)) {
            val binTail = bin.drop(1)
            decoders(ver).decode(binTail.unsafeArrayBuffer)
          } else if (ver < 0)
            throw js.JavaScriptException("Bad data")
          else
            unsupportedVer(ver, latestVer)
        }

      Async[A](
        encode = encoder.encode,
        decode = binary.decode(_).flatMap(decode))
    }

    def pickleCompressEncrypt[A](c: Compression, e: Encryption)(implicit pickler: SafePickler[A]): BinaryLayer[A] =
      _
        .encrypt(e) // Encryption is the very last step
        .compress(c) // Here we compress the binary *before* encrypting
        .pickle[A]
  }

}
