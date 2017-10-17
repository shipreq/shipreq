package shipreq.base.util

import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.univeq.UnivEq
import scala.collection.GenTraversableOnce
import scalaz.{-\/, Semigroup, \/, \/-}
import Min2Set.Maybe

/**
 * A set with at least 2 distinct elements.
 *
 * @param tail Does NOT contain head.
 */
final class Min2Set[A] private[util] (val head: A, val tail: NonEmptySet[A]) {
  private[this] implicit def univEq: UnivEq[A] = UnivEq.force

  override def toString = MutableArray(whole).map(_.toString).sort.mkString("Min2Set(", ", ", ")")

  override def hashCode = whole.##

  override def equals(o: Any) = o match {
    case that: Min2Set[_] => this.whole == that.whole
    case _ => false
  }

  def size: Int =
    tail.size + 1

  def iterator: Iterator[A] =
    whole.iterator

  def whole: Set[A] =
    tail.whole + head

  def contains(a: A): Boolean =
    (head == a) || (tail contains a)

  def lacks(a: A): Boolean =
    !contains(a)

  def map[B: UnivEq](f: A => B): Maybe[B] =
    Min2Set(toNES map f)

  def foreach[U](f: A => U): Unit = {
    f(head)
    tail foreach f
  }

  def exists(f: A => Boolean): Boolean =
    f(head) || tail.exists(f)

  def +(a: A): Min2Set[A] =
    if (contains(a))
      this
    else
      new Min2Set(head, tail + a)

  def ++(as: GenTraversableOnce[A]): Min2Set[A] =
    new Min2Set(head, as.foldLeft(tail)((q, a) => if (a == head) q else q + a))

  def ++(as: Min2Set[A]): Min2Set[A] =
    ++(as.whole)

  def last: A =
    tail.last

  def foldLeft[B](z: B)(f: (B, A) => B): B =
    tail.foldLeft(f(z, head))(f)

  def foldMapLeft1[B](g: A => B)(f: (B, A) => B): B =
    tail.foldLeft(g(head))(f)

  def reduceMapLeft1[B](f: A => B)(g: (B, B) => B): B =
    foldMapLeft1(f)((b, a) => g(b, f(a)))

  def reduce[B >: A](f: (B, B) => B): B =
    reduceMapLeft1[B](a => a)(f)

  def toNES: NonEmptySet[A] =
    tail + head

  def toNEV: NonEmptyVector[A] =
    NonEmptyVector(head, tail.whole.toVector)

  def toMin2Vector: Min2Vector[A] =
    Min2Vector(head, tail.toNEV)
}

// =====================================================================================================================

object Min2Set {
  type Maybe[A] = NonEmptySet[A] \/ Min2Set[A]

  def apply[A: UnivEq](h1: A, h2: A, t: A*): Maybe[A] =
    apply(NonEmptySet(h1, t.toSet + h2))

  def apply[A: UnivEq](s: NonEmptySet[A]): Maybe[A] =
    NonEmptySet.maybe(s.tail, -\/(s): Maybe[A])(t =>
      \/-(new Min2Set(s.head, t)))

  def maybe[A: UnivEq, B](s: Set[A], zero: => B)(one: A => B)(f: Min2Set[A] => B): B =
    NonEmptySet.maybe(s, zero)(maybe1(_)(one)(f))

  def maybe1[A: UnivEq, B](s: NonEmptySet[A])(one: A => B)(f: Min2Set[A] => B): B =
   apply(s).fold(o => one(o.head), f)

  def force[A: UnivEq](as: Set[A]): Min2Set[A] =
    new Min2Set(as.head, NonEmptySet force as.tail)

  def unwrapOption[A](o: Option[Min2Set[A]]): Set[A] =
    o.fold(Set.empty[A])(_.whole)

  implicit def univEq[A: UnivEq]: UnivEq[Min2Set[A]] = UnivEq.force

  implicit def semigroup[A]: Semigroup[Min2Set[A]] =
    new Semigroup[Min2Set[A]] {
      override def append(a: Min2Set[A], b: => Min2Set[A]): Min2Set[A] = a ++ b
    }
}
