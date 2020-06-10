package shipreq.base.util

import scala.scalajs.js.{UndefOr, undefined}
import scalaz.{Monoid, Semigroup}

object UndefOrExt {

  def append[A](fa: UndefOr[A], fb: UndefOr[A])(m: (A, A) => A): UndefOr[A] =
    fa.fold(fb)(a => fb.fold(fa)(m(a, _)))

  implicit def undefOrMonoid[A: Semigroup]: Monoid[UndefOr[A]] =
    new Monoid[UndefOr[A]] {
      override def zero = undefined
      override def append(fa: UndefOr[A], fb: => UndefOr[A]) = UndefOrExt.append(fa, fb)(Semigroup[A].append(_, _))
    }
}
