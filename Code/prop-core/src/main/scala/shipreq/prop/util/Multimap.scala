package shipreq.base.util

import shipreq.base.util.Multimap.Multiness

class Multimap[K, L[_], V](val m: Map[K, L[V]], l: Multiness[L]) {

  def copy(m: Map[K, L[V]]) = new Multimap[K, L, V](m, l)

  def apply(k: K) = m.getOrElse(k, l.e)

  def mod(k: K, f: L[V] => L[V]) = copy(m + (k -> f(this(k))))

  def add(k: K, v: V) = mod(k, l.add1(_, v))

  def addN(k: K, vs: L[V]) = mod(k, l.addN(_, vs))

  @inline final def +(kv: (K, V)) = add(kv._1, kv._2)
  @inline final def ++(kv: (K, L[V])) = addN(kv._1, kv._2)

  override def toString = m.toString()
  override def hashCode = m.hashCode
  @inline final def plainMap = m
}

object Multimap {
  
  trait Multiness[L[_]] {
    final def apply[K, V] = new Multimap[K, L, V](Map.empty, this)
    def e[V]: L[V]
    def add1[V](a: L[V], b: V): L[V]
    def addN[V](a: L[V], b: L[V]): L[V]
  }

  object list extends Multiness[List] {
    override def e[V] = List.empty[V]
    override def add1[V](a: List[V], b: V) = b :: a
    override def addN[V](a: List[V], b: List[V]) = b ::: a
  }

  object set extends Multiness[Set] {
    override def e[V] = Set.empty[V]
    override def add1[V](a: Set[V], b: V) = a + b
    override def addN[V](a: Set[V], b: Set[V]) = a ++ b
  }
}