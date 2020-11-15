package shipreq.webapp.member.protocol.indexeddb

import japgolly.scalajs.react.AsyncCallback
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import shipreq.base.util.BinaryData
import shipreq.base.util.JsExt._
import shipreq.webapp.member.protocol.binary.Compression

final case class IndexedDbCodec[A](encode: A => AsyncCallback[js.Any],
                                   decode: Any => AsyncCallback[A]) {

  def xmap[B](onDecode: A => B)(onEncode: B => A): IndexedDbCodec[B] =
    IndexedDbCodec[B](
      encode = b => encode(onEncode(b)),
      decode = d => decode(d).map(onDecode),
    )

  def compress(c: Compression)(implicit ev: IndexedDbCodec[A] =:= IndexedDbCodec[BinaryData]): IndexedDbCodec[BinaryData] =
    ev(this).xmap(c.decompressOrThrow)(c.compress)

//  TODO def encrypt(implicit ev: IndexedDbCodec[A] =:= IndexedDbCodec[BinaryData]): IndexedDbCodec[BinaryData] = {
//    val self = ev(this)
//  }

//  TODO def pickle[B](implicit ev: IndexedDbCodec[A] =:= IndexedDbCodec[BinaryData]): IndexedDbCodec[B] = {
//    val self = ev(this)
//  }
}

object IndexedDbCodec {

  val binary: IndexedDbCodec[BinaryData] =
    apply(
      encode = b => AsyncCallback.pure(b.unsafeArrayBuffer),
      decode = d => AsyncCallback.delay(
        d match {
          case ab: ArrayBuffer => BinaryData.unsafeFromArrayBuffer(ab)
          case x               => throw new RuntimeException("ArrayBuffer expected: " + x)
        }
      )
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
}
