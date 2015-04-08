package shipreq.base.util

import japgolly.nyaya.util._
import scalaz._
import scalaz.std.anyVal.intInstance

/**
 * Universal equality.
 */
trait UnivEq[A] extends Equal[A] {
  final override def equalIsNatural = true
  final override def equal(a: A, b: A) = a == b
}

object UnivEq {
  @inline def apply[F](implicit u: UnivEq[F]): UnivEq[F] = u

  private[this] val instance = new UnivEq[Any] {}

  @inline def force[A]: UnivEq[A] = instance.asInstanceOf[UnivEq[A]]

  @inline implicit def string : UnivEq[String]  = force
  @inline implicit def long   : UnivEq[Long]    = force
  @inline implicit def int    : UnivEq[Int]     = force
  @inline implicit def short  : UnivEq[Short]   = force
  @inline implicit def boolean: UnivEq[Boolean] = force

  @inline implicit def set   [A: UnivEq]           : UnivEq[Set[A]]          = force
  @inline implicit def list  [A: UnivEq]           : UnivEq[List[A]]         = force
  @inline implicit def vector[A: UnivEq]           : UnivEq[Vector[A]]       = force
  @inline implicit def map   [K: UnivEq, V: UnivEq]: UnivEq[Map[K, V]]       = force
  @inline implicit def disj  [A: UnivEq, B: UnivEq]: UnivEq[A \/ B]          = force
  @inline implicit def nel   [A: UnivEq]           : UnivEq[NonEmptyList[A]] = force

  @inline implicit def multimap[K, L[_], V](implicit ev: UnivEq[Map[K, L[V]]]): UnivEq[Multimap[K, L, V]] = force

  @inline implicit def oneAnd[F[_], A](implicit fa: UnivEq[F[A]], a: UnivEq[A]): UnivEq[OneAnd[F, A]] = force

  def withOrder[A](o: Order[A]): Order[A] with UnivEq[A] =
    new Order[A] with UnivEq[A] {
      override def order(a: A, b: A) = o.order(a, b)
    }

  def withArbitraryOrder[A](values: Iterable[A]): Order[A] with UnivEq[A] = {
    val fixedOrder = values.zipWithIndex.toMap
    new Order[A] with UnivEq[A] {
      @inline private[this] def int(s: A) = fixedOrder(s)
      override def order(a: A, b: A) = Order[Int].order(int(a), int(b))
    }
  }

  @inline def emptyMap     [K: UnivEq, V]                    = Map.empty[K, V]
  @inline def emptySet     [A: UnivEq]                       = Set.empty[A]
  @inline def emptyMultimap[K: UnivEq, L[_]: MultiValues, V] = Multimap.empty[K, L, V]
}