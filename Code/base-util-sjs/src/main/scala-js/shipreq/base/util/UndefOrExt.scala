package shipreq.base.util

import scalaz.{Monoid, Semigroup}
import scalajs.js.{UndefOr, undefined}

object UndefOrExt {

  def append[A](fa: UndefOr[A], fb: UndefOr[A])(m: (A, A) => A): UndefOr[A] =
    fa.fold(fb)(a => fb.fold(fa)(m(a, _)))

  implicit def undefOrMonoid[A: Semigroup]: Monoid[UndefOr[A]] =
    new Monoid[UndefOr[A]] {
      override def zero = undefined
      override def append(fa: UndefOr[A], fb: => UndefOr[A]) = UndefOrExt.append(fa, fb)(Semigroup[A].append(_, _))
    }
}
