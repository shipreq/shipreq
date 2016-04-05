package shipreq.base.util

import japgolly.univeq.UnivEq
import scala.collection.GenTraversableOnce
import scala.math.Ordering
import scalaz._
import scalaz.std.vector.{vectorEqual, vectorOrder}

/**
 * A vector with at least 2 distinct elements.
 */
final class Min2Vector[+A](val head: A, val tail: NonEmptyVector[A]) {
  override def toString = "NonEmpty" + whole.toString

  override def hashCode = head.## * 31 + tail.##

  override def equals(o: Any) = o match {
    case that: Min2Vector[Any] => this.head == that.head && this.tail == that.tail
    case _ => false
  }

  def length: Int =
    tail.length + 1

  def apply(i: Int): Option[A] =
    if (i == 0)
      Some(head)
    else
      tail(i - 1)

  def init: Vector[A] =
    head +: tail.init

  def map[B](f: A => B): Min2Vector[B] =
    Min2Vector(f(head), tail map f)

  def flatMap[B](f: A => Min2Vector[B]): Min2Vector[B] =
    reduceMapLeft1(f)(_ ++ _)

  def foreach[U](f: A => U): Unit = {
    f(head)
    tail foreach f
  }

  def exists(f: A => Boolean): Boolean =
    f(head) || tail.exists(f)

  def mapTail[B >: A](f: NonEmptyVector[A] => NonEmptyVector[B]): Min2Vector[B] =
    Min2Vector(head, f(tail))

  def :+[B >: A](a: B): Min2Vector[B] =
    mapTail(_ :+ a)

  def +:[B >: A](a: B): Min2Vector[B] =
    Min2Vector(a, head +: tail)

  def ++[B >: A](as: GenTraversableOnce[B]): Min2Vector[B] =
    mapTail(_ ++ as)

  def ++[B >: A](b: Min2Vector[B]): Min2Vector[B] =
    ++(b.whole)

  def ++:[B >: A](as: Vector[B]): Min2Vector[B] =
    if (as.isEmpty) this else Min2Vector(as.head, as.tail ++: toNEV)

  def last: A =
    tail.last

  def whole: Vector[A] =
    head +: tail.whole

  def reverse: Min2Vector[A] =
    Min2Vector.end(tail.reverse, head)

  def foldLeft[B](z: B)(f: (B, A) => B): B =
    tail.foldLeft(f(z, head))(f)

  def foldMapLeft1[B](g: A => B)(f: (B, A) => B): B =
    tail.foldLeft(g(head))(f)

  def reduceMapLeft1[B](f: A => B)(g: (B, B) => B): B =
    foldMapLeft1(f)((b, a) => g(b, f(a)))

  def reduce[B >: A](f: (B, B) => B): B =
    reduceMapLeft1[B](a => a)(f)

  // Reduce bullshit red in IntelliJ
  def traverseD[L, B](f: A => L \/ B): L \/ Min2Vector[B] =
    Min2Vector.traverse1.traverseU(this)(f)

  def intercalate[B >: A](b: B): Min2Vector[B] =
    intercalateF(b)(a => a)

  def intercalateF[B](b: B)(f: A => B): Min2Vector[B] =
    Min2Vector force1 toNEV.intercalateF(b)(f)

//  def filter(f: A => Boolean): Option[Min2Vector[A]] =
//    Min2Vector.option(whole filter f)
//
//  def filterNot(f: A => Boolean): Option[Min2Vector[A]] =
//    filter(!f(_))

  def toStream = whole.toStream

  def toNES[B >: A : UnivEq]: NonEmptySet[B] =
    tail.toNES[B] + head

  def toNEV: NonEmptyVector[A] =
    head +: tail

  private def safeTrans[B](f: Vector[A] => Vector[B]): Min2Vector[B] =
    Min2Vector force f(whole)

  def sorted[B >: A](implicit ord: Ordering[B])       = safeTrans(_.sorted[B])
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]) = safeTrans(_ sortBy f)
  def sortWith(lt: (A, A) => Boolean)                 = safeTrans(_ sortWith lt)
}

// =====================================================================================================================

object Min2Vector extends Min2VectorImplicits0 {
  def two[A](h: A, t: A): Min2Vector[A] =
    new Min2Vector(h, NonEmptyVector one t)

  def apply[A](h1: A, h2: A, t: A*): Min2Vector[A] =
    apply(h1, NonEmptyVector(h2, t: _*))

  def apply[A](h: A, t: NonEmptyVector[A]): Min2Vector[A] =
    new Min2Vector(h, t)

//  def endOV[A](init: Option[Vector[A]], last: A): Min2Vector[A] =
//    init.fold(one(last))(end(_, last))
//
//  def endO[A](init: Option[Min2Vector[A]], last: A): Min2Vector[A] =
//    init.fold(one(last))(_ :+ last)

  def end[A](init: NonEmptyVector[A], last: A): Min2Vector[A] =
    Min2Vector(init.head, NonEmptyVector.end(init.tail, last))

  def maybe[A, B](v: Vector[A], empty: => B)(one: A => B)(f: Min2Vector[A] => B): B =
    NonEmptyVector.maybe(v, empty)(maybe1(_)(one)(f))

  def maybe1[A, B](v: NonEmptyVector[A])(one: A => B)(f: Min2Vector[A] => B): B =
    NonEmptyVector.maybe(v.tail, one(v.head))(t => f(Min2Vector(v.head, t)))

//  def option[A](v: NonEmptyVector[A]): Option[Min2Vector[A]] =
//    maybe[A, Option[Min2Vector[A]]](v, None)(Some.apply)

  def force[A](v: Vector[A]): Min2Vector[A] =
    apply(v.head, NonEmptyVector force v.tail)

  def force1[A](v: NonEmptyVector[A]): Min2Vector[A] =
    apply(v.head, NonEmptyVector force v.tail)

  def unwrapOption[A](o: Option[Min2Vector[A]]): Vector[A] =
    o.fold(Vector.empty[A])(_.whole)

  implicit def univEq[A: UnivEq]: UnivEq[Min2Vector[A]] = UnivEq.force

  implicit def semigroup[A]: Semigroup[Min2Vector[A]] =
    new Semigroup[Min2Vector[A]] {
      override def append(a: Min2Vector[A], b: => Min2Vector[A]): Min2Vector[A] = a ++ b
    }

  implicit val traverse1: Traverse1[Min2Vector] = new Traverse1[Min2Vector] {
    override def traverse1Impl[G[_], A, B](fa: Min2Vector[A])(f: A => G[B])(implicit ap: Apply[G]): G[Min2Vector[B]] =
      ap.map(NonEmptyVector.traverse1.traverse1Impl(fa.toNEV)(f))(force1)
    override def foldMapRight1[A, B](fa: Min2Vector[A])(z: A => B)(f: (A, => B) => B): B =
      fa.init.reverseIterator.foldLeft(z(fa.last))((b, a) => f(a, b))
  }
}

trait Min2VectorImplicits1 {
  implicit def order[A: Order]: Order[Min2Vector[A]] =
    vectorOrder[A].contramap(_.whole)
}

trait Min2VectorImplicits0 extends Min2VectorImplicits1 {
  implicit def equality[A: Equal]: Equal[Min2Vector[A]] =
    vectorEqual[A].contramap(_.whole)
}