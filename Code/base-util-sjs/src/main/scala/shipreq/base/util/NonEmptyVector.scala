package shipreq.base.util

import scala.collection.GenTraversableOnce
import scalaz.{Equal, OneAnd}
import scalaz.std.vector.vectorEqual

object NonEmptyVector {
  @inline def apply[A](h: A, t: A*): NonEmptyVector[A] =
    OneAnd(h, t.toVector)

  @inline def apply[A](h: A, t: Vector[A]): NonEmptyVector[A] =
    OneAnd(h, t)

  def end[A](init: Vector[A], last: A): NonEmptyVector[A] =
    if (init.isEmpty)
      OneAnd(last, Vector.empty)
    else
      OneAnd(init.head, init.tail :+ last)

  @inline def maybe[A, B](v: Vector[A], empty: => B)(f: NonEmptyVector[A] => B): B =
    if (v.isEmpty) empty else f(OneAnd(v.head, v.tail))

  @inline def option[A](v: Vector[A]): Option[NonEmptyVector[A]] =
    maybe[A, Option[NonEmptyVector[A]]](v, None)(Some.apply)

  // ===============================================================================================
  implicit def equality[A: Equal]: Equal[NonEmptyVector[A]] =
    OneAnd.oneAndEqual[Vector, A]

  // ===============================================================================================
  @inline implicit class NEVOps[A](val self: NonEmptyVector[A]) extends AnyVal {
    @inline def modt(f: Vector[A] => Vector[A]): NonEmptyVector[A] =
      OneAnd(self.head, f(self.tail))

    @inline def :+(a: A): NonEmptyVector[A] =
      modt(_ :+ a)

    @inline def +:(a: A): NonEmptyVector[A] =
      OneAnd(a, self.head +: self.tail)

    @inline def ++(as: GenTraversableOnce[A]): NonEmptyVector[A] =
      modt(_ ++ as)

    @inline def ++(b: NonEmptyVector[A]): NonEmptyVector[A] =
      ++(b.whole)

    def ++:(as: Vector[A]): NonEmptyVector[A] =
      if (as.isEmpty) self else OneAnd(as.head, as.tail ++ whole)

    def whole: Vector[A] =
      self.head +: self.tail

    def foldLeft[B](z: B)(f: (B, A) => B): B =
      self.tail.foldLeft(f(z, self.head))(f)

    def foldMapLeft1[B](g: A => B)(f: (B, A) => B): B =
      self.tail.foldLeft(g(self.head))(f)

    def reduceMapLeft1[B](f: A => B)(g: (B, B) => B): B =
      foldMapLeft1(f)((b, a) => g(b, f(a)))

    @inline def toStream = whole.toStream
  }

}