package shipreq.webapp.base.data

import scalaz.{OneAnd, Equal}
import scalaz.Isomorphism.<=>
import shapeless.contrib.scalaz.Instances._
import shipreq.base.util.TaggedTypes._


final case class Rev(value: Long) extends TaggedLong {
  @inline def succ      = Rev(value + 1L)
  @inline def +(r: Rev) = Rev(value + r.value)
}


sealed trait Alive
case object Alive extends Alive with (Boolean <=> Alive) {
  implicit val equality = Equal.equalA[Alive]
  override val from     = equality.equal(Alive, _: Alive)
  override val to       = if (_: Boolean) Alive else Dead
}
case object Dead extends Alive


sealed trait ImplicationRequired
case object ImplicationRequired extends ImplicationRequired with (Boolean <=> ImplicationRequired) {
  implicit val equality = Equal.equalA[ImplicationRequired]
  override val from     = equality.equal(ImplicationRequired, _: ImplicationRequired)
  override val to       = if (_: Boolean) ImplicationRequired else Not
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
 * An intensional subset over F[A].
 */
sealed abstract class ISubset[F[_], A] {
//  final def filter: Option[A => Boolean] = {
//    @inline def check(a: A, as: OneAnd[Set, A]) = a == as.head || as.tail.contains(a)
//    this match {
//      case Subset.All()    => None
//      case Subset.Only(as) => Some(check(_, as))
//      case Subset.Not(as)  => Some(!check(_, as))
//    }
//  }
}
object ISubset {
  final case class All [F[_], A]()                     extends ISubset[F, A]
  final case class Only[F[_], A](values: OneAnd[F, A]) extends ISubset[F, A]
  final case class Not [F[_], A](values: OneAnd[F, A]) extends ISubset[F, A]

  implicit def equality[F[_], A](implicit FA: Equal[F[A]],  A: Equal[A]) = deriveEqual[ISubset[F, A]]
}