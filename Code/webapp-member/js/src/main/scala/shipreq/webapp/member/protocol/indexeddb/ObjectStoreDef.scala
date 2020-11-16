package shipreq.webapp.member.protocol.indexeddb

import japgolly.scalajs.react.{AsyncCallback, CallbackTo}
import scala.scalajs.js

sealed trait ObjectStoreDef[A] {
  val name: String
}

object ObjectStoreDef {

  final case class Sync[A](name: String, codec: IndexedDbCodec[A]) extends ObjectStoreDef[A]

  final case class Async[A](name: String, codec: IndexedDbCodec.Async[A]) extends ObjectStoreDef[A] { self =>

    type Value = Async.Value {
      type DataType = A
      val store: self.type
    }

    def encode(a: A): AsyncCallback[Value] =
      codec.encode(a).map(value)

    def value(v: js.Any): Value =
      new Async.Value {
        override type DataType = A
        override val store: self.type = self
        override val value = v
      }

    val sync: Sync[Value] = {
      val codec = IndexedDbCodec[Value](
        encode = v => CallbackTo.pure(v.value),
        decode = v => CallbackTo.pure(value(v)),
      )
      Sync(name, codec)
    }
  }

  object Async {

    sealed trait Value {
      type DataType
      val store: Async[DataType]
      val value: js.Any

      final def decode: AsyncCallback[DataType] =
        store.codec.decode(value)
    }

  }

}