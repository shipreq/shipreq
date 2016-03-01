package shipreq.base.util

import scalaz.Isomorphism.<=>
import IsoBool._

/**
 * Boolean isomorphism.
 *
 * Mix into the base type and override [[this.companion]] there.
 */
trait IsoBool[B <: IsoBool[B]] extends (Boolean <=> B) with Product with Serializable {
  this: B =>

  def companion: Object[B]

  final def unary_! : B =
    if (this == companion.positive)
      companion.negative
    else
      companion.positive

  final val :: : B => Boolean = _ == this
  final val <~ : Boolean => B = if (_) this else !this

  final override def from = ::
  final override def to   = <~

  final def when[A](b: Boolean <=> A): A => B =
    a => this <~ (b from a)

  final def <=>[A <: IsoBool[A]](A: IsoBool[A]): B <=> A =
    new (B <=> A) {
      override val from: A => B = IsoBool.this when A
      override val to  : B => A = A when IsoBool.this
    }
}

object IsoBool {

  /**
   * Mix into the companion object for the type.
   */
  trait Object[B <: IsoBool[B]] {
    implicit final def equality = UnivEq.force[B]

    def positive: B with IsoBool[B]
    def negative: B with IsoBool[B]

    final def memo[A](f: B => A): B => A = {
      val p = f(positive)
      val n = f(negative)
      b => if (b :: positive) p else n
    }

    final def forall(f: B => Boolean): Boolean =
      f(positive) && f(negative)

    final def exists(f: B => Boolean): Boolean =
      f(positive) || f(negative)
  }

  /**
   * Adds boolean ops with `companion.positive` being the equivalent of `true`.
   */
  trait WithBoolOps[B <: IsoBool[B]] extends IsoBool[B] {
    this: B =>

    final def &(that: => B): B = {
      val pos = companion.positive
      pos <~ ((this :: pos) && (that :: pos))
    }

    final def |(that: => B): B = {
      val pos = companion.positive
      pos <~ ((this :: pos) || (that :: pos))
    }
  }
}
