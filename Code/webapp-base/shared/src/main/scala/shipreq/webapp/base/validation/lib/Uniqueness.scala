package shipreq.webapp.base.validation.lib

import cats.Eq
import shipreq.base.util.OptionalConflict
import shipreq.webapp.base.validation.lib.Implicits._
import shipreq.webapp.base.validation.lib.Simple._

object Uniqueness {

  /** Utilities that are useful in preparing data for uniqueness checking */
  object Util {
    def excludeKey[A, K: Eq](keyToExclude: K, data: IterableOnce[A])(getKey: A => K): Iterator[A] =
      data.iterator.filter(getKey(_) =!= keyToExclude)

    def excludeOptionalKey[A, K: Eq](keyToExclude: Option[K], data: IterableOnce[A])(getKey: A => K): Iterator[A] =
      keyToExclude match {
        case None    => data.iterator
        case Some(k) => excludeKey(k, data)(getKey)
      }
  }

  // ===================================================================================================================

  def notUnique = Invalidity("Already in use.")

  def apply[A] = new Builder[A]

  final class Builder[A] private[Uniqueness]() {
    // final class Builder[A] private[Uniqueness](private val unit: Unit) extends AnyVal { -- https://issues.scala-lang.org/browse/SI-9646

    /** Ensure A is unique amongst a collection of Bs */
    def apply[B](data: () => IterableOnce[B])
                (conflict: (B, A) => Boolean, ignore: B => Boolean): Invalidator[A] =
      Invalidator.test[A](
        v => data().iterator.forall(i => !conflict(i, v) || ignore(i)),
        notUnique)

    /** Convenience for common pattern of data being key/value tuples */
    def tuple[K, V](data: () => IterableOnce[(K, V)])
                   (conflict: (V, A) => Boolean, ignore: K => Boolean): Invalidator[A] =
      apply(data)((x, a) => conflict(x._2, a), x => ignore(x._1))
  }

  /** Ensure A doesn't exist in a collection of As */
  def within[A: Eq](data: => IterableOnce[A]): Invalidator[A] =
    Invalidator.test[A](a => data.iterator.forall(a =!= _), notUnique)

  /** Ensure A doesn't exist in a set of As */
  def set[A: UnivEq](data: => Set[A]): Invalidator[A] =
    Invalidator.test[A](!data.contains(_), notUnique)

  def string(data: => IterableOnce[String]): Invalidator[String] =
    Invalidator.test[String](s => data.iterator.forall(s !=* _), notUnique)

  def stringIgnoreCase(data: => IterableOnce[String]): Invalidator[String] =
    Invalidator.test[String](s0 => {
      val s = s0.toLowerCase
      data.iterator.forall(s !=* _.toLowerCase)
    }, notUnique)

  // ===================================================================================================================

  def keyWithValue[K: Eq, V: Eq](data: () => IterableOnce[(K, V)])(key: K): Invalidator[V] =
    apply[V].tuple(data)(Eq[V].eqv, _ === key)

  def optionalKeyWithValue[K: Eq, V: Eq](data: () => IterableOnce[(Option[K], V)])(key: Option[K]): Invalidator[V] =
    keyWithValue(data)(key)(OptionalConflict.equalOption, implicitly)

  def keyWithValueSet[K: Eq, V: UnivEq](data: () => IterableOnce[(K, Set[V])])(key: K): Invalidator[V] =
    apply[V].tuple(data)(_ contains _, _ === key)

  def optionalKeyWithValueSet[K: Eq, V: UnivEq](data: () => IterableOnce[(Option[K], Set[V])])(key: Option[K]): Invalidator[V] =
    keyWithValueSet(data)(key)(OptionalConflict.equalOption, implicitly)
}
