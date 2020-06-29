package shipreq.base.util

import scala.collection.View
import scala.collection.immutable.ArraySeq
import scalaz.{Applicative, Foldable, Monoid}

object ScalazExtra {

  // So we don't pull everything into the Scala.JS output
  implicit lazy val applicativeFunction0: Applicative[Function0] =
    new Applicative[Function0] {
      override def point[A]  (a: => A)                            : () => A = () => a
      override def ap   [A, B](fa: => () => A)(f: => () => A => B): () => B = () => f()(fa())
      override def map  [A, B](fa: () => A)(f: A => B)            : () => B = () => f(fa())
    }

  implicit lazy val iteratorFoldable: Foldable[Iterator] =
    new Foldable[Iterator] {
      def foldMap[A, B](fa: Iterator[A])(f: A => B)(implicit F: Monoid[B]) = foldLeft(fa, F.zero)((x, y) => Monoid[B].append(x, f(y)))
      def foldRight[A, B](fa: Iterator[A], b: => B)(f: (A, => B) => B)     = fa.foldRight(b)(f(_, _))
      override def foldLeft[A, B](fa: Iterator[A], b: B)(f: (B, A) => B)   = fa.foldLeft(b)(f)
      override def any[A](fa: Iterator[A])(p: A => Boolean)                = fa.exists(p)
      override def all[A](fa: Iterator[A])(p: A => Boolean)                = fa.forall(p)
    }

  // TODO Should probably do a similar thing app-wide to reduce JS size
  implicit lazy val setFoldable: Foldable[Set] =
    new Foldable[Set] {
      def foldMap[A, B](fa: Set[A])(f: A => B)(implicit F: Monoid[B]) = foldLeft(fa, F.zero)((x, y) => Monoid[B].append(x, f(y)))
      def foldRight[A, B](fa: Set[A], b: => B)(f: (A, => B) => B)     = fa.foldRight(b)(f(_, _))
      override def foldLeft[A, B](fa: Set[A], b: B)(f: (B, A) => B)   = fa.foldLeft(b)(f)
      override def any[A](fa: Set[A])(p: A => Boolean)                = fa.exists(p)
      override def all[A](fa: Set[A])(p: A => Boolean)                = fa.forall(p)
    }

  implicit lazy val foldableArraySeq: Foldable[ArraySeq] =
    new Foldable[ArraySeq] {
      override def foldMap[A, B](fa: ArraySeq[A])(f: A => B)(implicit F: Monoid[B]): B = {
        var b = F.zero
        var i = 0
        while (i < fa.length) {
          val a = fa(i)
          i += 1
          b = F.append(b, f(a))
        }
        b
      }

      override def foldRight[A, B](fa: ArraySeq[A], z: => B)(f: (A, => B) => B): B = {
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

      override def any[A](fa: ArraySeq[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def all[A](fa: ArraySeq[A])(p: A => Boolean): Boolean =
        fa.forall(p)
    }

  implicit lazy val applicativeView = new Applicative[View] {
    override def point[A](a: => A): View[A] = View(a)
    override def ap[A, B](fa: => View[A])(ff: => View[A => B]): View[B] =
      for {
        a <- fa
        f <- ff
      } yield f(a)
  }
}
