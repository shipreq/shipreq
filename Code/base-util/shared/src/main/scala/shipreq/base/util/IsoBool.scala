package shipreq.base.util

import japgolly.univeq.UnivEq
import monocle.{Iso, Lens}
import scala.collection.Factory
import scalaz.Isomorphism.<=>
import scalaz.{Monoid, Semigroup}
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

  @inline final def is(b: B): Boolean =
    b == this

  @inline final def when(cond: Boolean): B =
    if (cond) this else !this

  final override val from = is(_)
  final override val to   = when(_)

  final def fnToThisWhen[A](f: A => Boolean): A => B =
    to compose f

  final def fnToThisWhen[A](b: Boolean <=> A): A => B =
    fnToThisWhen(b.from)

  final def <=>[A <: IsoBool[A]](A: IsoBool[A]): B <=> A =
    new (B <=> A) {
      override val from: A => B = IsoBool.this fnToThisWhen A
      override val to  : B => A = A fnToThisWhen IsoBool.this
    }

  final def isoWhen(b: Boolean): Iso[B, Boolean] =
    if (b) Iso(from)(to) else (!this).isoWhen(true)

  final def isoWhen[A <: IsoBool[A]](a: A): Iso[A, B] =
    Iso[A, B](
      i => if (i.is(a)) this else !this)(
      i => if (i.is(this)) a else !a)

  final def whenAllAre(bs: B*): B =
    this when bs.forall(is)

  final def whenAnyAre(bs: B*): B =
    this when bs.exists(is)
}

object IsoBool {

  /**
   * Mix into the companion object for the type.
   */
  trait Object[B <: IsoBool[B]] {
    implicit final def equality: UnivEq[B] = UnivEq.force

    def positive: B with IsoBool[B]
    def negative: B with IsoBool[B]

    final def memo[A](f: B => A): B => A = {
      val p = f(positive)
      val n = f(negative)
      b => if (b is positive) p else n
    }

    final def memoLazy[A](f: B => A): B => A = {
      lazy val p = f(positive)
      lazy val n = f(negative)
      b => if (b is positive) p else n
    }

    final def fold[A](a: A)(f: (A, B) => A): A =
      f(f(a, positive), negative)

    final def mapReduce[X, Y](m: B => X)(r: (X, X) => Y): Y =
      r(m(positive), m(negative))

    final def foreach[A](f: B with IsoBool[B] => A): Unit = {
      f(positive)
      f(negative)
      ()
    }

    final def forall(f: B => Boolean): Boolean =
      f(positive) && f(negative)

    final def exists(f: B => Boolean): Boolean =
      f(positive) || f(negative)

    final type Values[+A] = IsoBool.Values[B, A]
    final object Values {
      def apply[A](f: B => A): Values[A] =
        IsoBool.Values(pos = f(positive), neg = f(negative))
      def both[A](a: A): Values[A] =
        IsoBool.Values(a, a)
      def lens[A](b: B): Lens[Values[A], A] =
        IsoBool.Values.lens(b)
      def partition[C[_], A](as: IterableOnce[A])(f: A => B)(implicit cbf: Factory[A, C[A]]): Values[C[A]] = {
        val b = Values(_ => cbf.newBuilder)
        for (a <- as.iterator) b(f(a)) += a
        b.map(_.result())
      }
    }
  }

  /**
   * Adds boolean ops with `companion.positive` being the equivalent of `true`.
   */
  trait WithBoolOps[B <: IsoBool[B]] extends IsoBool[B] {
    this: B =>

    final def &(that: => B): B = {
      val pos = companion.positive
      pos when ((this is pos) && (that is pos))
    }

    final def &&(that: => Boolean): B = {
      val pos = companion.positive
      pos when ((this is pos) && that)
    }

    final def |(that: => B): B = {
      val pos = companion.positive
      pos when ((this is pos) || (that is pos))
    }

    final def ||(that: => Boolean): B = {
      val pos = companion.positive
      pos when ((this is pos) || that)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class Values[B <: IsoBool[B], +A](pos: A, neg: A) {
    def apply(b: B): A =
      if (b is b.companion.positive) pos else neg
    def set[AA >: A](b: B, a: AA): Values[B, AA] =
      if (b is b.companion.positive) copy(pos = a) else copy(neg = a)
    def mod[AA >: A](b: B, f: A => AA): Values[B, AA] =
      if (b is b.companion.positive) copy(pos = f(pos)) else copy(neg = f(neg))
    def map[C](f: A => C): Values[B, C] =
      Values(pos = f(pos), neg = f(neg))
    def ap[C, D](other: Values[B, C])(f: (A, C) => D): Values[B, D] =
      Values(pos = f(pos, other.pos), neg = f(neg, other.neg))
    def exists(f: A => Boolean): Boolean =
      f(pos) || f(neg)
    def forall(f: A => Boolean): Boolean =
      f(pos) && f(neg)
  }

  trait ValuesLowPri {
    implicit def monoid[B <: IsoBool[B], A](implicit A: Monoid[A]): Monoid[Values[B, A]] =
      new Monoid[Values[B, A]] {
        override def zero = Values(A.zero, A.zero)
        override def append(x: Values[B, A], y: => Values[B, A]) = x.ap(y)(A.append(_, _))
      }
  }

  object Values extends ValuesLowPri {
    implicit def semigroup[B <: IsoBool[B], A](implicit A: Semigroup[A]): Semigroup[Values[B, A]] =
      new Semigroup[Values[B, A]] {
        override def append(x: Values[B, A], y: => Values[B, A]) = x.ap(y)(A.append(_, _))
      }

    implicit def univEq[B <: IsoBool[B], A: UnivEq]: UnivEq[Values[B, A]] =
      UnivEq.derive

    def lens[B <: IsoBool[B], A](b: B): Lens[Values[B, A], A] =
      if (b is b.companion.positive)
        Lens[Values[B, A], A](_.pos)(a => _.copy(pos = a))
      else
        Lens[Values[B, A], A](_.neg)(a => _.copy(neg = a))
  }
}
