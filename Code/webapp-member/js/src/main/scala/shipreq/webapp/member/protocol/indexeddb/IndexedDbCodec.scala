package shipreq.webapp.member.protocol.indexeddb

import japgolly.scalajs.react.{AsyncCallback, CallbackTo}
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import shipreq.base.util.BinaryData
import shipreq.base.util.JsExt._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.member.protocol.binary.{Compression, Encryption}

final case class IndexedDbCodec[A](encode: A => CallbackTo[js.Any],
                                   decode: js.Any => CallbackTo[A]) {

  def xmap[B](onDecode: A => B)(onEncode: B => A): IndexedDbCodec[B] =
    // Delegating because decoding can fail and must be wrapped to be pure
    xmapSync(
      a => CallbackTo(onDecode(a)))(
      b => CallbackTo(onEncode(b)))

  def xmapSync[B](onDecode: A => CallbackTo[B])(onEncode: B => CallbackTo[A]): IndexedDbCodec[B] =
    IndexedDbCodec[B](
      encode = onEncode(_).flatMap(encode),
      decode = decode(_).flatMap(onDecode))

  def async: IndexedDbCodec.Async[A] =
    IndexedDbCodec.Async(
      encode = encode.andThen(_.asAsyncCallback),
      decode = decode.andThen(_.asAsyncCallback))

  type ThisIsBinary = IndexedDbCodec[A] =:= IndexedDbCodec[BinaryData]

  def compress(c: Compression)(implicit ev: ThisIsBinary): IndexedDbCodec[BinaryData] =
    ev(this).xmap(c.decompressOrThrow)(c.compress)

  def pickle[B](implicit pickler: SafePickler[B], ev: ThisIsBinary): IndexedDbCodec[B] =
    ev(this).xmap(pickler.decodeOrThrow)(pickler.encode)
}

object IndexedDbCodec {

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
  }

  // ===================================================================================================================

  val binary: IndexedDbCodec[BinaryData] =
    apply(
      encode = b => CallbackTo.pure(b.unsafeArrayBuffer),
      decode = d => CallbackTo(BinaryData.unsafeFromArrayBuffer(d.asInstanceOf[ArrayBuffer]))
    )

  lazy val string: IndexedDbCodec[String] =
    apply(
      encode = s => CallbackTo.pure(s),
      decode = d => CallbackTo(
        (d: Any) match {
          case s: String => s
          case x         => throw new RuntimeException("String expected: " + x)
        }
      )
    )

  def default[A](c: Compression, e: Encryption)(implicit pickler: SafePickler[A]): Async[A] =
    binary
      .async
      .encrypt(e) // Encryption is the very last step
      .compress(c) // Here we compress the binary *before* encrypting
      .pickle[A]
}
