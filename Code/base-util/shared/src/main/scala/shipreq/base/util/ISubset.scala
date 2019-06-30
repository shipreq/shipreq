package shipreq.base.util

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.univeq.UnivEq
import scala.collection.TraversableLike

/**
 * An intensional subset over any `Set[A]`.
 */
sealed abstract class ISubset[A] {
  import ISubset._

  final def apply[F[X] <: TraversableLike[X, F[X]]](i: F[A]): F[A] =
    i filter this.filter

  @inline def contains = filter

  final val filter: A => Boolean =
    filterO getOrElse (_ => true)

  final def filterO: Option[A => Boolean] =
    this match {
      case All()    => None
      case Only(as) => Some(as.contains)
      case Not(as)  => Some(as.lacks)
    }

  final def remove(a: A): ISubset[A] = {
    @inline implicit def univEqA: UnivEq[A] = UnivEq.force
    this match {
      case Only(as) if as contains a => NonEmptySet.maybe(as.whole - a, All(): ISubset[A])(Only(_))
      case Not (as) if as contains a => NonEmptySet.maybe(as.whole - a, All(): ISubset[A])(Not (_))
      case b                         => b
    }
  }

  final def toSet: Set[A] =
    this match {
      case All()   => Set.empty
      case Only(v) => v.whole
      case Not (v) => v.whole
    }
}

object ISubset {
  final case class All [A]()                       extends ISubset[A]
  final case class Only[A](values: NonEmptySet[A]) extends ISubset[A]
  final case class Not [A](values: NonEmptySet[A]) extends ISubset[A]

  @inline implicit def univEquality[A](implicit v: UnivEq[NonEmptySet[A]]): UnivEq[ISubset[A]] =
    UnivEq.derive
}