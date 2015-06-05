package shipreq.base.util

import scala.collection.TraversableLike

/**
 * An intensional subset over any `Set[A]`.
 */
sealed abstract class ISubset[A] {
  final def apply[F[X] <: TraversableLike[X, F[X]]](i: F[A]): F[A] =
    i filter this.filter

  final def filter: A => Boolean =
    filterO getOrElse (_ => true)

  final def filterO: Option[A => Boolean] = {
    this match {
      case ISubset.All()    => None
      case ISubset.Only(as) => Some(as.contains)
      case ISubset.Not(as)  => Some(as.lacks)
    }
  }
}
object ISubset {
  final case class All [A]()                       extends ISubset[A]
  final case class Only[A](values: NonEmptySet[A]) extends ISubset[A]
  final case class Not [A](values: NonEmptySet[A]) extends ISubset[A]

  @inline implicit def univEquality[A](implicit v: UnivEq[NonEmptySet[A]]): UnivEq[ISubset[A]] =
    UnivEq.force
}