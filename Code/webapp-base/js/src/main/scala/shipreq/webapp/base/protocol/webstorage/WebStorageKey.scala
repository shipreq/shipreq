package shipreq.webapp.base.protocol.webstorage

import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import shipreq.webapp.base.protocol.webstorage.AbstractWebStorage.Key

final case class WebStorageKey[V](key: Key, valueCodec: ValueCodec[V]) {

  def get(implicit s: AbstractWebStorage): CallbackTo[Option[V]] =
    s.getItem(key).flatMap(CallbackTo.traverseOption(_)(valueCodec.decode))

  def set(value: V)(implicit s: AbstractWebStorage): Callback =
    valueCodec.encode(value).flatMap(s.setItem(key, _))

  def remove(implicit s: AbstractWebStorage): Callback =
    s.removeItem(key)

  def setOrRemove(value: Option[V])(implicit s: AbstractWebStorage): Callback =
    value.fold(remove)(set(_))
}

object WebStorageKey {

  def string(key: String): WebStorageKey[String] =
    new WebStorageKey(Key(key), ValueCodec.string)

  def boolean(key: String): WebStorageKey[Boolean] =
    new WebStorageKey(Key(key), ValueCodec.boolean)

  // ===================================================================================================================

  final case class Async[V](key: Key, valueCodec: ValueCodec.Async[V]) {

    def get(implicit s: AbstractWebStorage): AsyncCallback[Option[V]] =
      s.getItem(key).asAsyncCallback.flatMap(AsyncCallback.traverseOption(_)(valueCodec.decode))

    def set(value: V)(implicit s: AbstractWebStorage): AsyncCallback[Unit] =
      valueCodec.encode(value).flatMap(s.setItem(key, _).asAsyncCallback)

    def remove(implicit s: AbstractWebStorage): Callback =
      s.removeItem(key)

    def setOrRemove(value: Option[V])(implicit s: AbstractWebStorage): AsyncCallback[Unit] =
      value.fold(remove.asAsyncCallback)(set(_))
  }

}
