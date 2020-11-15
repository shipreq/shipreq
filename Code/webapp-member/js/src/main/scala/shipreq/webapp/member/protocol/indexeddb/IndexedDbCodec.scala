package shipreq.webapp.member.protocol.indexeddb

import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import shipreq.base.util.BinaryData
import shipreq.base.util.JsExt._
import shipreq.webapp.member.protocol.binary.Compression

trait IndexedDbCodec[A] { self =>

  val encode       : A => js.Any
  val decodeOrThrow: Any => A

  final def xmap[B](onDecode: A => B)(onEncode: B => A): IndexedDbCodec[B] =
    new IndexedDbCodec[B] {
      override val encode        = b => self.encode(onEncode(b))
      override val decodeOrThrow = d => onDecode(self.decodeOrThrow(d))
    }

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

  object Binary extends IndexedDbCodec[BinaryData] {

    override val encode =
      _.unsafeArrayBuffer

    override val decodeOrThrow = {
      case ab: ArrayBuffer => BinaryData.unsafeFromArrayBuffer(ab)
      case x               => throw new RuntimeException("ArrayBuffer expected: " + x)
    }
  }

  object String extends IndexedDbCodec[String] {

    override val encode =
      s => s

    override val decodeOrThrow = {
      case s: String => s
      case x         => throw new RuntimeException("String expected: " + x)
    }
  }

}
