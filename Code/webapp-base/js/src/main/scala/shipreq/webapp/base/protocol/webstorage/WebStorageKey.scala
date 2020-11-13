package shipreq.webapp.base.protocol.webstorage

import japgolly.scalajs.react.{Callback, CallbackTo}

final class WebStorageKey[A](key: String, write: A => String, read: String => Option[A]) {

  def get(implicit s: AbstractWebStorage): CallbackTo[Option[A]] =
    s.getItem(key).map(_.flatMap(read))

  def set(value: A)(implicit s: AbstractWebStorage): Callback =
    s.setItem(key, write(value))

  def remove(implicit s: AbstractWebStorage): Callback =
    s.removeItem(key)

  def setOrRemove(value: Option[A])(implicit s: AbstractWebStorage): Callback =
    value.fold(remove)(set(_))

  def map[B](f: A => Option[B])(g: B => A): WebStorageKey[B] =
    new WebStorageKey(key, write compose g, read(_).flatMap(f))

  def xmap[B](f: A => B)(g: B => A): WebStorageKey[B] =
    map(f.andThen(Some(_)))(g)
}

object WebStorageKey {

  def string(key: String): WebStorageKey[String] =
    new WebStorageKey(key, identity, Some(_))

  def boolean(key: String): WebStorageKey[Boolean] =
    string(key).map({
      case "1" => Some(true)
      case "0" => Some(false)
      case _   => None
    })({
      case true  => "1"
      case false => "0"
    })
}
