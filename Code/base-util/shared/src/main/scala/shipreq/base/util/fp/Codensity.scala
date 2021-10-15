package shipreq.base.util.fp

import cats.{Applicative, Monad}

abstract class Codensity[F[_], +A] { self =>

  def apply[B](f: A => F[B]): F[B]

  final def lower[AA >: A](implicit F: Applicative[F]): F[AA] =
    apply(F.point(_))

  final def map[B](f: A => B): Codensity[F, B] =
    new Codensity[F, B] {
      override def apply[C](g: B => F[C]): F[C] =
        self.apply(g compose f)
    }

  final def flatMap[B](f: A => Codensity[F, B]): Codensity[F, B] =
    new Codensity[F, B] {
      override def apply[C](g: B => F[C]): F[C] =
        self.apply(f(_)(g))
    }

  final def flatTap[B](f: A => Codensity[F, B]): Codensity[F, A] =
    new Codensity[F, A] {
      override def apply[C](g: A => F[C]): F[C] =
        self.apply(a => f(a)(_ => g(a)))
    }

  final def andReturn[B](b: B): Codensity[F, B] =
    map(_ => b)
}


object Codensity {

  def pure[F[_], A](a: A): Codensity[F, A] =
    new Codensity[F, A] {
      override def apply[B](f: A => F[B]): F[B] =
        f(a)
    }

  def point[F[_], A](a: => A): Codensity[F, A] =
    new Codensity[F, A] {
      override def apply[B](f: A => F[B]): F[B] =
        f(a)
    }

  def lift[F[_], A](fa: F[A])(implicit F: Monad[F]): Codensity[F, A] =
    new Codensity[F, A] {
      override def apply[B](f: A => F[B]): F[B] =
        F.flatMap(fa)(f)
    }
}