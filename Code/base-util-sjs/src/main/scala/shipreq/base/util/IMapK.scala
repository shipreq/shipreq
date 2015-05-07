package shipreq.base.util

import japgolly.nyaya.util.MultiValues
import scalaz.{Equal, Order}

/**
 * Proof that a relationship between F and G always holds for any A < T.
 *
 * ∀α. ∃(Fα → Gα)
 */
trait RelationProof[T, F[+_ <: T], G[+_ <: T]] {
  def apply[A <: T](f: F[A]): G[A]

  @inline def forceCastK[A <: T, K[_]](k: K[F[T]]): K[F[A]] =
    k.asInstanceOf[K[F[A]]]

  def emptyIMapK(implicit ev: UnivEq[G[T]]): IMapK[T, G, F] =
    IMapK.empty[T, G, F](this)
}

// =====================================================================================================================

object IMapK {
  implicit def equality[T, K[+_ <: T], V[+_ <: T]](implicit k: Order[K[T]], v: Equal[V[T]]): Equal[IMapK[T, K, V]] =
    IMapBaseV.equality[K[T], V[T], IMapK[T, K, V]]

  def empty[T, K[+_ <: T], V[+_ <: T]](nt: RelationProof[T, V, K])(implicit k: UnivEq[K[T]]): IMapK[T, K, V] =
    new IMapK[T, K, V](nt, UnivEq.emptyMap)
}

final class IMapK[T, K[+_ <: T], V[+_ <: T]] private (rel: RelationProof[T, V, K], m: Map[K[T], V[T]])
                                                     (implicit ev: UnivEq[K[T]]) extends IMapBase[K[T], V[T], IMapK[T, K, V]](m) {
  type GK = K[T]
  type GV = V[T]

  override protected def stringPrefix           = "IMapK"
  override protected def repr                   = this
  override protected def setmap(n: Map[GK, GV]) = new IMapK[T, K, V](rel, n)
  override protected def _gkey(v: GV)           = rel(v)

  def get[A <: T](k: K[A]): Option[V[A]] =
    rel.forceCastK(m get k)

  def apply[A <: T](k: K[A]): Must[V[A]] =
    Must.fromOption(get(k), s"Value not found for $k. Keys = $keySet.")
}

// =====================================================================================================================

object ISetMapK {
  implicit def equality[T, K[+_ <: T], V[+_ <: T]](implicit k: Order[K[T]], v: UnivEq[V[T]]): Equal[ISetMapK[T, K, V]] =
    IMapBaseV.equality[K[T], Set[V[T]], ISetMapK[T, K, V]](implicitly, UnivEq.set)

  def empty[T, K[+_ <: T], V[+_ <: T]](nt: RelationProof[T, V, K])(implicit ek: UnivEq[K[T]], ev: UnivEq[V[T]]): ISetMapK[T, K, V] =
    new ISetMapK[T, K, V](nt, UnivEq.emptyMap)
}

final class ISetMapK[T, K[+ _ <: T], V[+ _ <: T]] private[util](rel: RelationProof[T, V, K], val m: Map[K[T], Set[V[T]]])
                                                               (implicit ek: UnivEq[K[T]], ev: UnivEq[V[T]])
  extends IMapBaseV[K[T], V[T], Set[V[T]], ISetMapK[T, K, V]](m) {

  type GK = K[T]
  type GV = V[T]
  type VS = Set[GV]

  override protected def stringPrefix                        = "ISetMapK"
  override protected def repr                                = this
  override protected def setmap(n: Map[GK, VS])              = new ISetMapK[T, K, V](rel, n)
  override protected def _gkey(v: GV)                        = rel(v)
  override protected def _values(vs: VS)                     = vs
  override protected def _add(to: Map[GK, VS], k: GK, v: GV) = to.updated(k, apply(k) + v)

  def apply[A <: T](k: K[A]): Set[V[A]] =
    rel.forceCastK(m.getOrElse(k, Set.empty[GV]))
}

// =====================================================================================================================

object IMultimapK {
  implicit def equality[T, K[+_ <: T], L[+_], V[+_ <: T]](implicit k: Order[K[T]], v: Equal[L[V[T]]]): Equal[IMultimapK[T, K, L, V]] =
    IMapBaseV.equality[K[T], L[V[T]], IMultimapK[T, K, L, V]]

  def empty[T, K[+_ <: T], L[+_], V[+_ <: T]](nt: RelationProof[T, V, K])(implicit ek: UnivEq[K[T]], L: MultiValues[L]): IMultimapK[T, K, L, V] =
    new IMultimapK[T, K, L, V](nt, UnivEq.emptyMap)
}

final class IMultimapK[T, K[+ _ <: T], L[+_], V[+ _ <: T]] private[util](rel: RelationProof[T, V, K], val m: Map[K[T], L[V[T]]])
                                                                        (implicit ek: UnivEq[K[T]], L: MultiValues[L])
  extends IMapBaseV[K[T], V[T], L[V[T]], IMultimapK[T, K, L, V]](m) {

  type GK = K[T]
  type GV = V[T]
  type VS = L[GV]

  override protected def stringPrefix                        = "IMultimapK"
  override protected def repr                                = this
  override protected def setmap(n: Map[GK, VS])              = new IMultimapK[T, K, L, V](rel, n)
  override protected def _gkey(v: GV)                        = rel(v)
  override protected def _values(vs: VS)                     = L.stream(vs)
  override protected def _add(to: Map[GK, VS], k: GK, v: GV) = to.updated(k, L.add1(_apply(k), v))

  private def _apply(k: GK): VS =
    m.getOrElse(k, L.empty[GV])

  def apply[A <: T](k: K[A]): L[V[A]] =
    rel.forceCastK(_apply(k))
}