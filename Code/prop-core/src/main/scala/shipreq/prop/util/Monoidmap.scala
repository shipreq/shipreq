package shipreq.base.util

import scalaz.Monoid

object Monoidmap {
  def empty[K, V: Monoid] = new Monoidmap[K, V](Map.empty)
}

class Monoidmap[K, V](val m: Map[K, V])(implicit M: Monoid[V]) {

  def copy(m: Map[K, V]) = new Monoidmap[K, V](m)

  def apply(k: K) = m.getOrElse(k, M.zero)

  def mod(k: K, f: V => V) = copy(m + (k -> f(this(k))))

  def add(k: K, v: => V) = mod(k, M.append(_, v))

  @inline final def +(kv: (K, V)) = add(kv._1, kv._2)

  override def toString = m.toString()
  override def hashCode = m.hashCode

  @inline final def plainMap = m
}
