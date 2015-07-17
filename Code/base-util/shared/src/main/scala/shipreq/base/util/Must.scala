package shipreq.base.util

import scalaz.{Monad, \/, \/-, -\/}

/**
 * A value that we believe must exist but cannot prove to exist via types.
 *
 * [[japgolly.nyaya]] should be used to assert the pertinent invariants.
 *
 * In the event that this fails, there will likely be side-effects in future.
 * Something like the
 */
sealed abstract class Must[+A] {
  def map    [B](f: A => B)                    : Must[B]
  def flatMap[B](f: A => Must[B])              : Must[B]
  def fold   [B](e: String => B, f: A => B)    : B
  def filter    (f: A => Boolean, e: => String): Must[A]

  final def toDisjunction: String \/ A =
    fold(-\/.apply, \/-.apply)
}

object Must {

  @inline def apply[A](value: A): Must[A] =
    Exists(value)

  final case class Exists[A](value: A) extends Must[A] {
    override def map    [B](f: A => B)                    : Must[B] = Exists(f(value))
    override def flatMap[B](f: A => Must[B])              : Must[B] = f(value)
    override def fold   [B](e: String => B, f: A => B)    : B       = f(value)
    override def filter    (f: A => Boolean, e: => String): Must[A] = if (f(value)) this else Failed(e)
  }

  final case class Failed(explanation: String) extends Must[Nothing] {
    override def map    [B](f: Nothing => B)                    : Must[B]       = this
    override def flatMap[B](f: Nothing => Must[B])              : Must[B]       = this
    override def fold   [B](e: String => B, f: Nothing => B)    : B             = e(explanation)
    override def filter    (f: Nothing => Boolean, e: => String): Must[Nothing] = this
  }

  implicit val scalazInstance: Monad[Must] =
    new Monad[Must] {
      override def point[A]   (a: => A)                     : Must[A] = Exists(a)
      override def map  [A, B](fa: Must[A])(f: A => B)      : Must[B] = fa map f
      override def bind [A, B](fa: Must[A])(f: A => Must[B]): Must[B] = fa flatMap f
    }

  object Auto {
    @inline implicit def autoMust[A](a: A): Must[A] = Exists(a)
  }

  def fromOption[A](o: Option[A], explanation: => String): Must[A] =
    o.fold[Must[A]](Failed(explanation))(Exists(_))

  def fromDisjunction[A](d: String \/ A): Must[A] =
    d.fold[Must[A]](Failed, Exists(_))

  def foldMap[A, B, Q](t: TraversableOnce[A], z: Q)(f: A => Must[B])(g: (Q, B) => Q): Must[Q] =
    t.foldLeft(Must(z))((mq, a) =>
      for {q <- mq; b <- f(a)} yield g(q, b))

  def foldMapM[A, M[_], B](t: TraversableOnce[A])(f: A => Must[B])(implicit m: Monoidish[M]): Must[M[B]] =
    foldMap(t, m.empty[B])(f)(m.add)

  def foldMapMF[A, M[_], B](t: TraversableOnce[A])(f: A => Must[M[B]])(implicit m: Monoidish[M]): Must[M[B]] =
    foldMap(t, m.empty[B])(f)(m.append)
}