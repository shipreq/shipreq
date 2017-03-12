package shipreq.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._

object FilterFn {

  type F[A] = A => Boolean

  val `n/a`: F[Any] = _ => true

  private def fuse[A](x: F[A], y: F[A])(f: (F[A], F[A]) => F[A]): F[A] =
    if (x eq `n/a`)
      y
    else if (y eq `n/a`)
      x
    else
      f(x, y)

  final case class Pair[A, B](fa: F[A], fb: F[B]) {

    def unary_! : Pair[A, B] =
      Pair(!fa, !fb)

    def &&(y: Pair[A, B]): Pair[A, B] =
      Pair(
        fuse(fa, y.fa)(_ && _),
        fuse(fb, y.fb)(_ && _))

    def ||(y: Pair[A, B]): Pair[A, B] =
      Pair(
        fuse(fa, y.fa)(_ || _),
        fuse(fb, y.fb)(_ || _))

    def contramap[A2, B2](ma: A2 => A, mb: B2 => B): Pair[A2, B2] =
      Pair(
        fa compose ma,
        fb compose mb)

    def contramapA[X](m: X => A): Pair[X, B] =
      Pair(fa compose m, fb)

    def contramapB[X](m: X => B): Pair[A, X] =
      Pair(fa, fb compose m)

  }
}
