package shipreq.base.util

import cats._
import scala.collection.View

object CatsExtra {

  // So we don't pull everything into the Scala.JS output
  implicit lazy val applicativeFunction0: Applicative[Function0] =
    new Applicative[Function0] {
      override def pure [A]   (a: A)                        : () => A = () => a
      override def ap   [A, B](f: () => A => B)(fa: () => A): () => B = () => f()(fa())
      override def map  [A, B](fa: () => A)(f: A => B)      : () => B = () => f(fa())
    }

  implicit lazy val iteratorFoldable: Foldable[Iterator] =
    new Foldable[Iterator] {
      override def exists[A](fa: Iterator[A])(p: A => Boolean)                              = fa.exists(p)
      override def foldLeft[A, B](fa: Iterator[A], b: B)(f: (B, A) => B)                    = fa.foldLeft(b)(f)
      override def foldMap[A, B](fa: Iterator[A])(f: A => B)(implicit F: Monoid[B])         = foldLeft(fa, F.empty)((x, y) => Monoid[B].combine(x, f(y)))
      override def foldRight[A, B](fa: Iterator[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]) = fa.foldRight(b)(f(_, _))
      override def forall[A](fa: Iterator[A])(p: A => Boolean)                              = fa.forall(p)
    }

  // TODO Should probably do a similar thing app-wide to reduce JS size
  implicit lazy val setFoldable: Foldable[Set] =
    new Foldable[Set] {
      override def exists[A](fa: Set[A])(p: A => Boolean)                              = fa.exists(p)
      override def foldLeft[A, B](fa: Set[A], b: B)(f: (B, A) => B)                    = fa.foldLeft(b)(f)
      override def foldMap[A, B](fa: Set[A])(f: A => B)(implicit F: Monoid[B])         = foldLeft(fa, F.empty)((x, y) => Monoid[B].combine(x, f(y)))
      override def foldRight[A, B](fa: Set[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]) = fa.foldRight(b)(f(_, _))
      override def forall[A](fa: Set[A])(p: A => Boolean)                              = fa.forall(p)
    }

  implicit lazy val foldableArraySeq: Foldable[ArraySeq] =
    new Foldable[ArraySeq] {
      override def foldMap[A, B](fa: ArraySeq[A])(f: A => B)(implicit F: Monoid[B]): B = {
        var b = F.empty
        var i = 0
        while (i < fa.length) {
          val a = fa(i)
          i += 1
          b = F.combine(b, f(a))
        }
        b
      }

      override def foldRight[A, B](fa: ArraySeq[A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        var b = z
        var i = fa.length
        while (i > 0) {
          i -= 1
          val a = fa(i)
          b = f(a, b)
        }
        b
      }

      override def foldLeft[A, B](fa: ArraySeq[A], z: B)(f: (B, A) => B): B = {
        var b = z
        var i = 0
        while (i < fa.length) {
          val a = fa(i)
          i += 1
          b = f(b, a)
        }
        b
      }

      override def exists[A](fa: ArraySeq[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: ArraySeq[A])(p: A => Boolean): Boolean =
        fa.forall(p)
    }

  implicit lazy val applicativeView: Applicative[View] =
    new Applicative[View] {
      override def pure[A](a: A): View[A] = View(a)
      override def ap[A, B](ff: View[A => B])(fa: View[A]): View[B] =
        for {
          a <- fa
          f <- ff
        } yield f(a)
    }

  @inline final implicit class ApplicativeDelay[F[_]](private val f: Applicative[F]) extends AnyVal {
    def delay[A](a: => A): F[A] =
      f.map(f.unit)(_ => a)
  }
}
