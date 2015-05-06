package shipreq.base.util

import scalaz.{Equal, Order}

object IMapK {
  implicit def equality[T, K[+_ <: T], V[+_ <: T]](implicit k: Order[K[T]], v: Equal[V[T]]): Equal[IMapK[T, K, V]] =
    IMapBase.equality[K[T], V[T], IMapK[T, K, V]]

  def empty[T, K[+_ <: T], V[+_ <: T]](nt: KeyProof[T, K, V])(implicit k: UnivEq[K[T]]): IMapK[T, K, V] =
    new IMapK[T, K, V](nt, Map.empty)

  trait KeyProof[T, K[+_ <: T], V[+_ <: T]] {
    def apply[A <: T](v: V[A]): K[A]
  }
}

final class IMapK[T, K[+_ <: T], V[+_ <: T]] private (key: IMapK.KeyProof[T, K, V], m: Map[K[T], V[T]])
                                                     (implicit ev: UnivEq[K[T]])  extends IMapBase[K[T], V[T], IMapK[T, K, V]](m) {
  type GK = K[T]
  type GV = V[T]

  override protected def stringPrefix = "IMapK"

  override protected def repr = this

  override protected def setmap(n: Map[GK, GV]) = new IMapK[T, K, V](key, n)

  override protected def gkey(v: GV) = key(v)

  @inline private def forceCastO[A <: T](o: Option[V[T]]): Option[V[A]] =
    o.asInstanceOf[Option[V[A]]]

  def get[A <: T](k: K[A]): Option[V[A]] =
    forceCastO(m get k)

  def apply[A <: T](k: K[A]): Must[V[A]] =
    Must.fromOption(get(k), s"Value not found for $k. Keys = $keySet.")
}
