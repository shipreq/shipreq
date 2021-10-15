package shipreq.base.util

import cats.{Monoid, Semigroup}
import scala.scalajs.js.{UndefOr, undefined}

object UndefOrExt {

  def combine[A](fa: UndefOr[A], fb: UndefOr[A])(m: (A, A) => A): UndefOr[A] =
    fa.fold(fb)(a => fb.fold(fa)(m(a, _)))

  implicit def undefOrMonoid[A: Semigroup]: Monoid[UndefOr[A]] =
    new Monoid[UndefOr[A]] {
      override def empty = undefined
      override def combine(fa: UndefOr[A], fb: UndefOr[A]) = UndefOrExt.combine(fa, fb)(Semigroup[A].combine(_, _))
    }
}
