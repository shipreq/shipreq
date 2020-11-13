package shipreq.webapp.base.protocol.webstorage

import japgolly.scalajs.react.{Callback, CallbackTo, Reusability}
import org.scalajs.dom.raw.{Storage => StorageJs}
import scala.scalajs.js
import shipreq.base.util.SetOnceVar

trait AbstractWebStorage {
  def clear: Callback
  def getItem(key: String): CallbackTo[Option[String]]
  def removeItem(key: String): Callback
  def setItem(key: String, data: String): Callback
  def getLength: CallbackTo[Int]
  def getKey(index: Int): CallbackTo[Option[String]]
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

  // ===================================================================================================================

  object AlwaysEmpty extends AbstractWebStorage {
    override def clear: Callback =
      Callback.empty

    override def getItem(key: String): CallbackTo[Option[String]] =
      CallbackTo.pure(None)

    override def removeItem(key: String): Callback =
      Callback.empty

    override def setItem(key: String, data: String): Callback =
      Callback.empty

    override def getLength: CallbackTo[Int] =
      CallbackTo.pure(0)

    override def getKey(index: Int): CallbackTo[Option[String]] =
      CallbackTo.pure(None)
  }

  // ===================================================================================================================

  private final class Real(storageJs: StorageJs) extends AbstractWebStorage {
    override def clear: Callback =
      Callback(storageJs.clear())

    override def getItem(key: String): CallbackTo[Option[String]] =
      CallbackTo(Option(storageJs.getItem(key)))

    override def removeItem(key: String): Callback =
      Callback(storageJs.removeItem(key))

    override def setItem(key: String, data: String): Callback =
      Callback(storageJs.setItem(key, data))

    override def getLength: CallbackTo[Int] =
      CallbackTo(storageJs.length)

    override def getKey(index: Int): CallbackTo[Option[String]] =
      CallbackTo(Option(storageJs.key(index)))
  }

  // ===================================================================================================================

  def inMemory() =
    new InMemory

  final class InMemory extends AbstractWebStorage {
    private var state = Map.empty[String, String]

    override def clear: Callback =
      Callback {state = Map.empty}

    override def getItem(key: String): CallbackTo[Option[String]] =
      CallbackTo(state.get(key))

    override def removeItem(key: String): Callback =
      Callback {state -= key}

    override def setItem(key: String, data: String): Callback =
      Callback {state = state.updated(key, data)}

    override def getLength: CallbackTo[Int] =
      CallbackTo(state.size)

    override def getKey(index: Int): CallbackTo[Option[String]] =
      CallbackTo(state.iterator.drop(index).nextOption().map(_._1))
  }

}