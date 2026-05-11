package shipreq.webapp.member.protocol.indexeddb

import japgolly.scalajs.react.{AsyncCallback, CallbackTo}
import org.scalajs.dom.IDBValue
import scala.scalajs.js.typedarray.ArrayBuffer
import shipreq.base.util.BinaryData
import shipreq.base.util.JsExt._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.member.protocol.binary.{BinaryFormat, Compression}

final case class ValueCodec[A](encode: A => CallbackTo[IDBValue],
                               decode: IDBValue => CallbackTo[A]) {

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

  final case class Async[A](encode: A => AsyncCallback[IDBValue],
                            decode: IDBValue => AsyncCallback[A]) {

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

    def xmapBinaryFormat[B](fmt: BinaryFormat[B])(implicit ev: ThisIsBinary): Async[B] =
      ev(this).xmapAsync(fmt.decode)(fmt.encode)
  }

  object Async {
    val binary = ValueCodec.binary.async
  }
}
