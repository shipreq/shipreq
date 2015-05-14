package shipreq.webapp.base.data

import scala.collection.TraversableLike
import scalaz.Isomorphism.<=>
import shipreq.base.util.TaggedTypes._
import shipreq.base.util.{NonEmptySet, UnivEq}


final case class Rev(value: Long) extends TaggedLong {
  @inline def succ      = Rev(value + 1L)
  @inline def +(r: Rev) = Rev(value + r.value)
}


sealed abstract class Alive {
//  def fold[A](alive: => A, dead: => A): A
}
case object Alive extends Alive with (Boolean <=> Alive) {
  @inline implicit def equality = UnivEq.force[Alive]
  override val from             = equality.equal(Alive, _: Alive)
  override val to               = if (_: Boolean) Alive else Dead

//  override def fold[A](alive: => A, dead: => A): A = alive
}
case object Dead extends Alive {
//  override def fold[A](alive: => A, dead: => A): A = dead
}


sealed trait ImplicationRequired
case object ImplicationRequired extends ImplicationRequired with (Boolean <=> ImplicationRequired) {
  @inline implicit def equality = UnivEq.force[ImplicationRequired]
  override val from             = equality.equal(ImplicationRequired, _: ImplicationRequired)
  override val to               = if (_: Boolean) ImplicationRequired else Not
  case object Not extends ImplicationRequired
}


/**
 * A key by which users can refer to data.
 * These references require a hashtag prefix.
 *
 * Examples:
 * #TBD refers to a custom issue type.
 * #pri=high refers to a grouping.
 */
final case class HashRefKey(value: String) extends TaggedString


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