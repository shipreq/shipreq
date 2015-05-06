package shipreq.base.util

import scalaz.{Equal, Order}

object IMap {
  implicit def equality[K: Order, V: Equal]: Equal[IMap[K, V]] =
    IMapBase.equality[K, V, IMap[K, V]]

  def empty[K: UnivEq, V](k: V => K): IMap[K, V] =
    new IMap(k, Map.empty)
}

final class IMap[K: UnivEq, V] private (key: V => K, m: Map[K, V]) extends IMapBase[K, V, IMap[K, V]](m) {

  override protected def stringPrefix = "IMap"

  override protected def repr = this

  override protected def setmap(n: Map[K, V]) = new IMap(key, n)

  override protected def gkey(v: V) = key(v)

  def get(k: K): Option[V] =
    m.get(k)

  def apply(k: K): Must[V] =
    Must.fromOption(get(k), s"Value not found for $k. Keys = $keySet.")

  def mod(k: K, f: V => V)(implicit ev: V <:< AnyRef): This =
    _mod(k, f, this)

  def modOrPut(k: K, f: V => V, put: => V)(implicit ev: V <:< AnyRef): This =
    _mod(k, f, this add put)

  private def _mod(k: K, f: V => V, nomod: => IMap[K, V])(implicit ev: V <:< AnyRef): This =
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
