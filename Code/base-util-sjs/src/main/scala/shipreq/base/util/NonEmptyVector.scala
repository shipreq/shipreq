package shipreq.base.util

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.math.Ordering
import scalaz._
import scalaz.std.vector.{vectorEqual, vectorOrder}

final class NonEmptyVector[+A](val head: A, val tail: Vector[A]) {
  override def toString = "NonEmpty" + whole.toString

  override def hashCode = head.## * 31 + tail.##

  override def equals(o: Any) = o match {
    case that: NonEmptyVector[Any] => this.head == that.head && this.tail == that.tail
    case _ => false
  }

  @inline def length: Int =
    tail.length + 1

  def apply(i: Int): Option[A] =
    if (i == 0)
      Some(head)
    else
      try {
        Some(tail(i - 1))
      } catch {
        case _: IndexOutOfBoundsException => None
      }

  def map[B](f: A => B): NonEmptyVector[B] =
    NonEmptyVector(f(head), tail map f)

  def flatMap[B](f: A => NonEmptyVector[B]): NonEmptyVector[B] =
    reduceMapLeft1(f)(_ ++ _)

  def foreach[U](f: A => U): Unit = {
    f(head)
    tail foreach f
  }

  def exists(f: A => Boolean): Boolean =
    f(head) || tail.exists(f)

  @inline def mapTail[B >: A](f: Vector[A] => Vector[B]): NonEmptyVector[B] =
    NonEmptyVector(head, f(tail))

  @inline def :+[B >: A](a: B): NonEmptyVector[B] =
    mapTail(_ :+ a)

  @inline def +:[B >: A](a: B): NonEmptyVector[B] =
    NonEmptyVector(a, head +: tail)

  @inline def ++[B >: A](as: GenTraversableOnce[B]): NonEmptyVector[B] =
    mapTail(_ ++ as)

  @inline def ++[B >: A](b: NonEmptyVector[B]): NonEmptyVector[B] =
    ++(b.whole)

  def ++:[B >: A](as: Vector[B]): NonEmptyVector[B] =
    if (as.isEmpty) this else NonEmptyVector(as.head, as.tail ++ whole)

  def last: A =
    if (tail.isEmpty) head else tail.last

  def whole: Vector[A] =
    head +: tail

  def reverse: NonEmptyVector[A] =
    if (tail.isEmpty) this else NonEmptyVector.end(tail.reverse, head)

  def foldLeft[B](z: B)(f: (B, A) => B): B =
    tail.foldLeft(f(z, head))(f)

  def foldMapLeft1[B](g: A => B)(f: (B, A) => B): B =
    tail.foldLeft(g(head))(f)

  def reduceMapLeft1[B](f: A => B)(g: (B, B) => B): B =
    foldMapLeft1(f)((b, a) => g(b, f(a)))

  def reduce[B >: A](f: (B, B) => B): B =
    reduceMapLeft1[B](a => a)(f)

  def intercalate[B >: A](b: B): NonEmptyVector[B] =
    intercalateF(b)(a => a)

  def intercalateF[B](b: B)(f: A => B): NonEmptyVector[B] = {
    val r = implicitly[CanBuildFrom[Nothing, B, Vector[B]]].apply()
    for (a <- tail) {
      r += b
      r += f(a)
    }
    NonEmptyVector(f(head), r.result())
  }

  @inline def toSet[B >: A] = whole.toSet[B]
  @inline def toStream      = whole.toStream

  @inline def toNonEmptySet[B >: A : UnivEq]: NonEmptySet[B] =
    NonEmptySet(head, tail.toSet[B])

  private def safeTrans[B](f: Vector[A] => Vector[B]): NonEmptyVector[B] = {
    val v = f(whole)
    NonEmptyVector(v.head, v.tail)
  }

  def sorted[B >: A](implicit ord: Ordering[B])       = safeTrans(_.sorted[B])
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]) = safeTrans(_ sortBy f)
  def sortWith(lt: (A, A) => Boolean)                 = safeTrans(_ sortWith lt)

  def partitionD[B, C](f: A => B \/ C): (NonEmptyVector[B], Vector[C]) \/ (Vector[B], NonEmptyVector[C]) = {
    var bs = Vector.empty[B]
    var cs = Vector.empty[C]
    for (a <- tail)
      f(a) match {
        case -\/(b) => bs :+= b
        case \/-(c) => cs :+= c
      }
    f(head) match {
      case -\/(b) => -\/((NonEmptyVector(b, bs), cs))
      case \/-(c) => \/-((bs, NonEmptyVector(c, cs)))
    }
  }

  def partitionB(f: A => Boolean): (NonEmptyVector[A], Vector[A]) = {
    var ts = Vector.empty[A]
    var fs = Vector.empty[A]
    for (a <- tail)
      if (f(a))
        ts :+= a
      else
        fs :+= a
    if (ts.nonEmpty)
      (NonEmptyVector(ts.head, ts.tail), fs)
    else
      (NonEmptyVector(fs.head, fs.tail), ts)
  }
}

// =====================================================================================================================

object NonEmptyVector extends NonEmptyVectorImplicits0 {
  @inline def one[A](h: A): NonEmptyVector[A] =
    new NonEmptyVector(h, Vector.empty)

  @inline def apply[A](h: A, t: A*): NonEmptyVector[A] =
    apply(h, t.toVector)

  @inline def apply[A](h: A, t: Vector[A]): NonEmptyVector[A] =
    new NonEmptyVector(h, t)

  def endOV[A](init: Option[Vector[A]], last: A): NonEmptyVector[A] =
    init.fold(one(last))(end(_, last))

  def endO[A](init: Option[NonEmptyVector[A]], last: A): NonEmptyVector[A] =
    init.fold(one(last))(_ :+ last)

  def end[A](init: Vector[A], last: A): NonEmptyVector[A] =
    if (init.isEmpty)
      one(last)
    else
      new NonEmptyVector(init.head, init.tail :+ last)

  @inline def maybe[A, B](v: Vector[A], empty: => B)(f: NonEmptyVector[A] => B): B =
    if (v.isEmpty) empty else f(NonEmptyVector(v.head, v.tail))

  @inline def option[A](v: Vector[A]): Option[NonEmptyVector[A]] =
    maybe[A, Option[NonEmptyVector[A]]](v, None)(Some.apply)

  def unwrapOption[A](o: Option[NonEmptyVector[A]]): Vector[A] =
    o.fold(Vector.empty[A])(_.whole)

  implicit def univEq[A: UnivEq]: UnivEq[NonEmptyVector[A]] = UnivEq.force

  implicit def semigroup[A]: Semigroup[NonEmptyVector[A]] =
    new Semigroup[NonEmptyVector[A]] {
      override def append(a: NonEmptyVector[A], b: => NonEmptyVector[A]): NonEmptyVector[A] = a ++ b
    }

  object Sole {
    def unapply[A](v: NonEmptyVector[A]) = new Unapply(v)
    final class Unapply[A](val v: NonEmptyVector[A]) extends AnyVal {
      def isEmpty = v.tail.nonEmpty
      def get     = v.head
    }
  }
}

trait NonEmptyVectorImplicits1 {
  implicit def order[A: Order]: Order[NonEmptyVector[A]] =
    vectorOrder[A].contramap(_.whole)
}

trait NonEmptyVectorImplicits0 extends NonEmptyVectorImplicits1 {
  implicit def equality[A: Equal]: Equal[NonEmptyVector[A]] =
    vectorEqual[A].contramap(_.whole)
}