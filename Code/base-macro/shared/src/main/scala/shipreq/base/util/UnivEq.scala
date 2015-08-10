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

sealed abstract class UnivEqImplicits {
  protected val instance = new UnivEq[Any] {}

  @inline protected def univEqForce[A]: UnivEq[A] = instance.asInstanceOf[UnivEq[A]]

  @inline implicit def univEqString : UnivEq[String]  = univEqForce
  @inline implicit def univEqLong   : UnivEq[Long]    = univEqForce
  @inline implicit def univEqInt    : UnivEq[Int]     = univEqForce
  @inline implicit def univEqShort  : UnivEq[Short]   = univEqForce
  @inline implicit def univEqBoolean: UnivEq[Boolean] = univEqForce

  @inline implicit def univEqOption[A: UnivEq]           : UnivEq[Option[A]]       = univEqForce
  @inline implicit def univEqSet   [A: UnivEq]           : UnivEq[Set[A]]          = univEqForce
  @inline implicit def univEqList  [A: UnivEq]           : UnivEq[List[A]]         = univEqForce
  @inline implicit def univEqVector[A: UnivEq]           : UnivEq[Vector[A]]       = univEqForce
  @inline implicit def univEqMap   [K: UnivEq, V: UnivEq]: UnivEq[Map[K, V]]       = univEqForce
  @inline implicit def univEqDisj  [A: UnivEq, B: UnivEq]: UnivEq[A \/ B]          = univEqForce
  @inline implicit def univEqThese [A: UnivEq, B: UnivEq]: UnivEq[A \&/ B]         = univEqForce
  @inline implicit def univEqNel   [A: UnivEq]           : UnivEq[NonEmptyList[A]] = univEqForce

  @inline implicit def univEqMultimap[K, L[_], V](implicit ev: UnivEq[Map[K, L[V]]]): UnivEq[Multimap[K, L, V]] = univEqForce

  @inline implicit def univEqOneAnd[F[_], A](implicit fa: UnivEq[F[A]], a: UnivEq[A]): UnivEq[OneAnd[F, A]] = univEqForce

  @inline implicit def univEqTuple2[A:UnivEq, B:UnivEq]: UnivEq[(A,B)] = univEqForce
  @inline implicit def univEqTuple3[A:UnivEq, B:UnivEq, C:UnivEq]: UnivEq[(A,B,C)] = univEqForce
  @inline implicit def univEqTuple4[A:UnivEq, B:UnivEq, C:UnivEq, D:UnivEq]: UnivEq[(A,B,C,D)] = univEqForce
  @inline implicit def univEqTuple5[A:UnivEq, B:UnivEq, C:UnivEq, D:UnivEq, E:UnivEq]: UnivEq[(A,B,C,D,E)] = univEqForce
  @inline implicit def univEqTuple6[A:UnivEq, B:UnivEq, C:UnivEq, D:UnivEq, E:UnivEq, F:UnivEq]: UnivEq[(A,B,C,D,E,F)] = univEqForce
  @inline implicit def univEqTuple7[A:UnivEq, B:UnivEq, C:UnivEq, D:UnivEq, E:UnivEq, F:UnivEq, G:UnivEq]: UnivEq[(A,B,C,D,E,F,G)] = univEqForce
  @inline implicit def univEqTuple8[A:UnivEq, B:UnivEq, C:UnivEq, D:UnivEq, E:UnivEq, F:UnivEq, G:UnivEq, H:UnivEq]: UnivEq[(A,B,C,D,E,F,G,H)] = univEqForce
  @inline implicit def univEqTuple9[A:UnivEq, B:UnivEq, C:UnivEq, D:UnivEq, E:UnivEq, F:UnivEq, G:UnivEq, H:UnivEq, I:UnivEq]: UnivEq[(A,B,C,D,E,F,G,H,I)] = univEqForce
}

object UnivEq extends UnivEqImplicits {
  @inline def apply[F](implicit u: UnivEq[F]): UnivEq[F] = u

  @inline def force[A]: UnivEq[A] =
    instance.asInstanceOf[UnivEq[A]]

  object Implicits extends UnivEqImplicits

  // -------------------------------------------------------------------------------------------------------------------

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

  // Copied from Shapeless
  trait =:!=[A, B]
  def _unexpected : Nothing = sys.error("Unexpected invocation")
  implicit def _neq[A, B] : A =:!= B = null.asInstanceOf[A =:!= B] //new =:!=[A, B] {}
  implicit def _neqAmbig1[A] : A =:!= A = _unexpected
  implicit def _neqAmbig2[A] : A =:!= A = _unexpected

  @inline def emptyMap        [K: UnivEq, V]         = Map.empty[K, V]
  @inline def emptySet        [A: UnivEq]            = Set.empty[A]
  @inline def emptySetMultimap[K: UnivEq, V: UnivEq] = Multimap.empty[K, Set, V]
  @inline def emptyMultimap   [K: UnivEq, L[_] : MultiValues, V](implicit ev: L[V] =:!= Set[V]) = Multimap.empty[K, L, V]

  @inline def mutableHashMapMemo[K: UnivEq, V](f: K => V)   = Memo.mutableHashMapMemo[K, V](f)
  @inline def immutableHashMapMemo[K: UnivEq, V](f: K => V) = Memo.immutableHashMapMemo[K, V](f)
}