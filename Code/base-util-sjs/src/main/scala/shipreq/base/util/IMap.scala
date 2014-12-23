package shipreq.base.util

import scala.annotation.elidable
import scalaz.{Order, Equal, Foldable}
import scalaz.std.iterable._
import scalaz.std.map._
import scalaz.syntax.foldable._

object IMap {
  implicit def equality[K: Order, V: Equal]: Equal[IMap[K, V]] =
    Equal.equalBy(_.underlyingMap)

  def empty[K, V](k: V => K): IMap[K, V] = new IMap(k, Map.empty)
}

final class IMap[K, V] private (key: V => K, m: Map[K, V]) {

  override def toString = s"I$m"
  override def hashCode = m.hashCode
  override def equals(o: Any) = o match {
    case n: IMap[_, _] => m equals n.underlyingMap
    case n: Map[_, _]  => m equals n
    case _             => false
  }

  @inline private[this] def setmap(n: Map[K, V]): IMap[K, V] = new IMap(key, n)

  def underlyingMap = m

  def keys = m.keys

  def values = m.values

  def keySet = m.keySet

  def mapValues[A](f: V => A): Map[K, A] = m.mapValues(f)

  def get(k: K): Option[V] = m.get(k)

  def -(k: K) = setmap(m - k)

  // ------------------------------------------------

  def add(v: V) = setmap(m.updated(key(v), v))

  def addAll(vs: V*) = addAllF(vs)

  def addAllF[F[_]: Foldable](vs: F[V]) = setmap(vs.foldLeft(m)((n, v) => n.updated(key(v), v)))

  def vstream[A](f: V => A): Stream[A] = values.toStream.map(f)
  def vstreamf[A](f: V => Stream[A]): Stream[A] = values.toStream.flatMap(f)

  @elidable(elidable.ASSERTION)
  def assertValidKeys(m: Map[K, V]): Unit =
    for ((k1,v) <- m)
      assert(key(v) == k1, s"Expected key for [$v] is [${key(v)}] but [$k1] was found.")

  def replaceUnderlying(n: Map[K, V]): IMap[K, V] = {
    assertValidKeys(n)
    setmap(n)
  }

  def mapUnderlying(f: Map[K, V] => Map[K, V]): IMap[K, V] =
    replaceUnderlying(f(m))

  def mod(k: K, f: V => V)(implicit ev: V <:< AnyRef): IMap[K, V] =
    _mod(k, f, this)

  def modOrPut(k: K, f: V => V, put: => V)(implicit ev: V <:< AnyRef): IMap[K, V] =
    _mod(k, f, this add put)

  private def _mod(k: K, f: V => V, nomod: => IMap[K, V])(implicit ev: V <:< AnyRef): IMap[K, V] =
    m.get(k).fold(nomod)(v => {
      val v2 = f(v)
      if (ev(v) eq ev(v2))
        this
      else {
        val k2 = key(v2)
        var n = m.updated(k2, v2)
        if (k != k2) n -= k
        setmap(n)
      }
    })

}

/*
import scalaz.{Order, ==>>}

object IMap {
  def empty[K, V](k: V => K): IMap[K, V] = new IMap(k, scalaz.IMap.empty)
}

final class IMap[K: Order, V] private (k: V => K, m: K ==>> V) {
  @inline private[this] def mod(n: K ==>> V): IMap[K, V] = new IMap(k, n)

  def add(v: V) = mod(m.insert(k(v), v))

  def addAll[F[_]: Foldable](vs: F[V]) = mod(vs.foldLeft(m)((n, v) => n.insert(k(v), v)))
}
*/