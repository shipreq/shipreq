package shipreq.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._

object FilterFn {

  type F[A] = A => Boolean

  val `n/a`: F[Any] = _ => true

  def fuse[A](x: F[A], y: F[A])(f: (F[A], F[A]) => F[A]): F[A] =
    if (x eq `n/a`)
      y
    else if (y eq `n/a`)
      x
    else
      f(x, y)
}

import FilterFn._

case class FilterFn2[A, B](a: F[A], b: F[B]) {
  final type This = FilterFn2[A, B]
  def unary_!    : This = FilterFn2(!a, !b)
  def &&(y: This): This = FilterFn2(a && y.a, fuse(b, y.b)(_ && _))
  def ||(y: This): This = FilterFn2(a || y.a, fuse(b, y.b)(_ || _))

  def contramap1[X](m: X => A): FilterFn2[X, B] = FilterFn2(a compose m, b)
  def contramap2[X](m: X => B): FilterFn2[A, X] = FilterFn2(a, b compose m)

  def contramap[A2, B2](ma: A2 => A, mb: B2 => B): FilterFn2[A2, B2] =
    FilterFn2[A2, B2](a compose ma, b compose mb)
}
