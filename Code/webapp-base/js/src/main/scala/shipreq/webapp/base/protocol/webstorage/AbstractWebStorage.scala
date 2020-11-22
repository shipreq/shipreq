package shipreq.webapp.base.protocol.webstorage

import japgolly.scalajs.react.{Callback, CallbackTo, Reusability}
import org.scalajs.dom.raw.{Storage => StorageJs}
import scala.scalajs.js
import shipreq.base.util.SetOnceVar

trait AbstractWebStorage {
  import AbstractWebStorage.{Key, Value}

  def clear: Callback
  def getItem(key: Key): CallbackTo[Option[Value]]
  def removeItem(key: Key): Callback
  def setItem(key: Key, data: Value): Callback
  def getLength: CallbackTo[Int]
  def getKey(index: Int): CallbackTo[Option[Key]]
}

object AbstractWebStorage {

  def apply(s: StorageJs): AbstractWebStorage =
    new Real(s)

  // Keep this as a def. It might be undefined at first, then later user grants access and it becomes available.
  // Is that a valid scenario? I don't know but may as well support it if possible.
  def local(): Option[AbstractWebStorage] = {
    val available = org.scalajs.dom.window.localStorage.asInstanceOf[js.UndefOr[StorageJs]]
    val option = available.toOption.flatMap(Option(_))
    option.map(ls => localStorageInstance.getOrSet(new Real(ls)))
  }

  private val localStorageInstance = SetOnceVar[Real]

  def localOrEmpty(): AbstractWebStorage =
    local().getOrElse(AlwaysEmpty)

  implicit def reusability: Reusability[AbstractWebStorage] =
    Reusability.byRef

  final case class Key(value: String) extends AnyVal

  final case class Value(value: String) extends AnyVal {
    def mod(f: String => String): Value =
      Value(f(value))
  }

  // ===================================================================================================================

  object AlwaysEmpty extends AbstractWebStorage {
    override def clear: Callback =
      Callback.empty

    override def getItem(key: Key): CallbackTo[Option[Value]] =
      CallbackTo.pure(None)

    override def removeItem(key: Key): Callback =
      Callback.empty

    override def setItem(key: Key, data: Value): Callback =
      Callback.empty

    override def getLength: CallbackTo[Int] =
      CallbackTo.pure(0)

    override def getKey(index: Int): CallbackTo[Option[Key]] =
      CallbackTo.pure(None)
  }

  // ===================================================================================================================

  private final class Real(storageJs: StorageJs) extends AbstractWebStorage {
    override def clear: Callback =
      Callback(storageJs.clear())

    override def getItem(key: Key): CallbackTo[Option[Value]] =
      CallbackTo {
        storageJs.getItem(key.value) match {
          case null => None
          case v    => Some(Value(v))
        }
      }

    override def removeItem(key: Key): Callback =
      Callback(storageJs.removeItem(key.value))

    override def setItem(key: Key, data: Value): Callback =
      Callback(storageJs.setItem(key.value, data.value))

    override def getLength: CallbackTo[Int] =
      CallbackTo(storageJs.length)

    override def getKey(index: Int): CallbackTo[Option[Key]] =
      CallbackTo {
        storageJs.key(index) match {
          case null => None
          case k    => Some(Key(k))
        }
      }
  }

  // ===================================================================================================================

  def inMemory() =
    new InMemory

  final class InMemory extends AbstractWebStorage {
    private var state = Map.empty[String, String]

    override def clear: Callback =
      Callback {state = Map.empty}

    override def getItem(key: Key): CallbackTo[Option[Value]] =
      CallbackTo(state.get(key.value).map(Value))

    override def removeItem(key: Key): Callback =
      Callback {state -= key.value}

    override def setItem(key: Key, data: Value): Callback =
      Callback {state = state.updated(key.value, data.value)}

    override def getLength: CallbackTo[Int] =
      CallbackTo(state.size)

    override def getKey(index: Int): CallbackTo[Option[Key]] =
      CallbackTo(state.iterator.drop(index).nextOption().map(e => Key(e._1)))
  }

}