package shipreq.webapp.base.lib

import japgolly.scalajs.react._
import scala.scalajs.js
import shipreq.webapp.base.jsfacade.LRUCache

final class LruCache[K, V](raw: LRUCache) {

  def get(k: K): CallbackTo[js.UndefOr[V]] =
    CallbackTo(raw.get(k.asInstanceOf[LRUCache.Key]).asInstanceOf[js.UndefOr[V]])

  def set(k: K, v: V): Callback =
    CallbackTo(raw.set(k, v))

  val reset: Callback =
    Callback(raw.reset())

  def memoAsync[I](key: I => K, f: (I, K) => AsyncCallback[V]): I => AsyncCallback[V] =
    i => {
      val k = key(i)
      get(k).asAsyncCallback.flatMap { cached =>
        if (cached.isDefined)
          AsyncCallback.pure(cached.get)
        else
          f(i, k).flatTapSync(set(k, _))
      }
    }
}

object LruCache {
  def apply[K, V] =
    new Builder[K, V](Nil)

  final class Builder[K, V](mods: List[LRUCache.Options => Unit]) {
    type This = Builder[K, V]

    private def add(f: LRUCache.Options => Unit): This =
      new Builder(f :: mods)

    def maxAgeMs(ms: Double): This =
      add(_.maxAge = ms)

    def maxSize(s: Double): This =
      add(_.max = s)

    def sizeBy(f: (K, V) => Double): This = {
      val g: LRUCache.Length = (v, k) => f(k.asInstanceOf[K], v.asInstanceOf[V])
      add(_.length = g)
    }

    def returnStaleValues(b: Boolean): This =
      add(_.stale = b)

    def updateAgeOnGet(b: Boolean): This =
      add(_.updateAgeOnGet = b)

    def build(): LruCache[K, V] = {
      val o = js.Object().asInstanceOf[LRUCache.Options]
      mods.foreach(_(o))
      val c = new LRUCache(o)
      new LruCache(c)
    }
  }

  final case class Result[+A](value: A)
}