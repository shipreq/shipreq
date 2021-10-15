package shipreq.base.util

import cats._
import cats.data.Const

trait TraverseWithDefaults[F[_]] extends Traverse[F] {

  override final def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B =
    traverse[Const[B, *], A, B](fa)(a => Const(f(a))).getConst

  private def andThenMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def combine(f: A => A, g: A => A) = f andThen g
    def empty: A => A                 = (a: A) => a
  }

  private def composeMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def combine(f: A => A, g: A => A) = f compose g
    def empty: A => A                 = (a: A) => a
  }

  private def defer[B](f: Eval[B] => Eval[B]): Eval[B] => Eval[B] =
    evalB => Eval.defer(f(evalB))

  override final def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    foldMap(fa)(f.curried andThen defer)(composeMonoid).apply(lb)
  }

  override final def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    foldMap[A, B => B](fa){a => b => f(b, a)}(andThenMonoid[B]).apply(b)
}
