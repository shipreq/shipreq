package shipreq.webapp.member.protocol.indexeddb

import japgolly.scalajs.react.AsyncCallback
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import shipreq.base.util.BinaryData
import shipreq.base.util.JsExt._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.member.protocol.binary.{Compression, Encryption}

final case class IndexedDbCodec[A](encode: A => AsyncCallback[js.Any],
                                   decode: Any => AsyncCallback[A]) {

  def xmap[B](onDecode: A => B)(onEncode: B => A): IndexedDbCodec[B] =
    // Delegating to xmapAsync because decoding can fail and must be wrapped in AsyncCallback to be pure.
    xmapAsync(
      a => AsyncCallback.delay(onDecode(a)))(
      b => AsyncCallback.delay(onEncode(b)))

  def xmapAsync[B](onDecode: A => AsyncCallback[B])(onEncode: B => AsyncCallback[A]): IndexedDbCodec[B] =
    IndexedDbCodec[B](
      encode = onEncode(_).flatMap(encode),
      decode = decode(_).flatMap(onDecode),
    )

  type ThisIsBinary = IndexedDbCodec[A] =:= IndexedDbCodec[BinaryData]

  def compress(c: Compression)(implicit ev: ThisIsBinary): IndexedDbCodec[BinaryData] =
    ev(this).xmap(c.decompressOrThrow)(c.compress)

  def encrypt(e: Encryption)(implicit ev: ThisIsBinary): IndexedDbCodec[BinaryData] =
    ev(this).xmapAsync(e.decrypt)(e.encrypt)

  def pickle[B](implicit pickler: SafePickler[B], ev: ThisIsBinary): IndexedDbCodec[B] =
    ev(this).xmap(pickler.decodeOrThrow)(pickler.encode)
}

object IndexedDbCodec {

  val binary: IndexedDbCodec[BinaryData] =
    apply(
      encode = b => AsyncCallback.pure(b.unsafeArrayBuffer),
      decode = d => AsyncCallback.delay(BinaryData.unsafeFromArrayBuffer(d.asInstanceOf[ArrayBuffer]))
    )

  lazy val string: IndexedDbCodec[String] =
    apply(
      encode = s => AsyncCallback.pure(s),
      decode = d => AsyncCallback.delay(
        d match {
          case s: String => s
          case x         => throw new RuntimeException("String expected: " + x)
        }
      )
    )

  def default[A](c: Compression, e: Encryption)(implicit pickler: SafePickler[A]): IndexedDbCodec[A] =
    binary
      .encrypt(e) // Encryption is the very last step
      .compress(c) // Here we compress the binary *before* encrypting
      .pickle[A]
}
