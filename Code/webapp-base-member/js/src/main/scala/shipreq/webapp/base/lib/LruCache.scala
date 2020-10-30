package shipreq.webapp.base.lib

import japgolly.scalajs.react._
import java.time.Duration
import scala.scalajs.js
import shipreq.webapp.base.jsfacade.LRUCache

final class LruCache[K, V](raw: LRUCache, loggerJs: LoggerJs) {
  import LruCache.Debug

  if (Debug) {
    console.log("Raw cache: ", raw)
  }

  def get(k: K): CallbackTo[js.UndefOr[V]] =
    CallbackTo(raw.get(k.asInstanceOf[LRUCache.Key]).asInstanceOf[js.UndefOr[V]])

  def set(k: K, v: V): Callback =
    CallbackTo {
      if (Debug) loggerJs(_.info(s"Adding to cache: ${k.##}"))
      raw.set(k, v)
    }

  val reset: Callback =
    Callback(raw.reset())

  def asyncGetOrSet(key: K, real: => AsyncCallback[V]): AsyncCallback[V] =
    get(key).asAsyncCallback.attempt.flatMap { result =>
      if (Debug) loggerJs(_.debug(s"Cache keys: ${raw.keys().toList.map(_.##).sorted.mkString("{", ", ", "}")}. Current key: ${key.##}"))
      result match {
        case Right(cached) if cached.isDefined =>
          if (Debug) loggerJs(_.info(s"Cache hit for ${key.##}"))
          AsyncCallback.pure(cached.get)
        case _ =>
          if (Debug) loggerJs(_.info(s"Cache miss for ${key.##}"))
          real.flatTapSync(set(key, _))
      }
    }

  def asyncGetOrSetX[A](key: K, real: => AsyncCallback[A])(f: A => V, g: V => A): AsyncCallback[A] =
    asyncGetOrSet(key, real.map(f)).map(g)

  def asyncGetOrSetR[A](key: K, real: => AsyncCallback[A])(implicit ev: V <:< LruCache.Result[Any]): AsyncCallback[A] = {
    val self = this.asInstanceOf[LruCache[K, LruCache.Result[Any]]]
    self.asyncGetOrSetX(key, real)(LruCache.Result(_), _.value.asInstanceOf[A])
  }

  def asyncMemo[I](key: I => K, f: (I, K) => AsyncCallback[V]): I => AsyncCallback[V] =
    i => {
      val k = key(i)
      asyncGetOrSet(k, f(i, k))
    }
}

object LruCache {

  final val Debug = false

  def apply[K, V] =
    new Builder[K, V](Nil, LoggerJs.off)

  def toAny[K] =
    apply[K, Result[Any]]

  type ToAny[K] = LruCache[K, Result[Any]]

  final class Builder[K, V](mods: List[LRUCache.Options => Unit], loggerJs: LoggerJs) {
    type This = Builder[K, V]

    private def add(f: LRUCache.Options => Unit): This =
      new Builder(f :: mods, loggerJs)

    def maxAge(dur: Duration): This =
      maxAgeMs(dur.toMillis.toDouble)

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

    def withLogger(l: LoggerJs): This =
      new Builder(mods, l)

    def build(): LruCache[K, V] = {
      val o = js.Object().asInstanceOf[LRUCache.Options]
      mods.foreach(_(o))
      val c = new LRUCache(o)
      new LruCache(c, loggerJs)
    }
  }

  final case class Result[+A](value: A)
}