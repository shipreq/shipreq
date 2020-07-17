package shipreq.base.util

import japgolly.microlibs.nonempty.NonEmpty
import scala.collection.{AbstractIterator, Factory}
import scala.math.Ordering
import scala.reflect.ClassTag
import scalaz.{Apply, Semigroup}

final class NonEmptyArraySeq[+A] private[NonEmptyArraySeq](val whole: ArraySeq[A]) {
  override def toString = "NonEmpty" + whole.toString

  override def hashCode = whole.hashCode

  override def equals(o: Any) = o match {
    case that: NonEmptyArraySeq[Any] => this.whole == that.whole
    case _ => false
  }

  @inline def unsafeApply(i: Int): A =
    whole(i)

  def apply(i: Int): Option[A] =
    try {
      Some(unsafeApply(i))
    } catch {
      case _: IndexOutOfBoundsException => None
    }

  @inline def head     = whole.head
  @inline def tail     = whole.tail
  @inline def init     = whole.init
  @inline def last     = whole.last
  @inline def length   = whole.length
  @inline def indices  = whole.indices
  @inline def iterator = whole.iterator

  def initNonEmpty: Option[NonEmptyArraySeq[A]] = NonEmptyArraySeq option init
  def tailNonEmpty: Option[NonEmptyArraySeq[A]] = NonEmptyArraySeq option tail

  def map[B](f: A => B): NonEmptyArraySeq[B] =
    new NonEmptyArraySeq(whole map f)

  def flatMap[B: ClassTag](f: A => NonEmptyArraySeq[B]): NonEmptyArraySeq[B] =
    if (length == 1)
      f(head)
    else {
      val b = ArraySeq.newBuilder[B]
      for (a <- this) {
        val bs = f(a)
        b ++= bs.whole
      }
      new NonEmptyArraySeq(b.result())
    }

  def foreach[U](f: A => U): Unit = {
    var i = 0
    while (i < whole.length) {
      val a = whole(i)
      i += 1
      f(a)
    }
  }

  def foreachWithIndex[U](f: (A, Int) => U): Unit = {
    var i = 0
    while (i < whole.length) {
      val a = whole(i)
      f(a, i)
      i += 1
    }
  }

  @inline def forall(f: A => Boolean): Boolean =
    whole.forall(f)

  @inline def exists(f: A => Boolean): Boolean =
    whole.exists(f)

  @inline def find(f: A => Boolean): Option[A] =
    whole.find(f)

  def mapWithIndex[B: ClassTag](f: (A, Int) => B): NonEmptyArraySeq[B] = {
    val n = new Array[B](length)
    var i = 0
    while (i < whole.length) {
      n(i) = f(whole(i), i)
      i += 1

    }
    new NonEmptyArraySeq(ArraySeq.unsafeWrapArray(n))
  }

  def :+[B >: A](a: B): NonEmptyArraySeq[B] =
    new NonEmptyArraySeq(whole :+ a)

  def +:[B >: A](a: B): NonEmptyArraySeq[B] =
    new NonEmptyArraySeq(a +: whole)

  def ++[B >: A](as: IterableOnce[B]): NonEmptyArraySeq[B] =
    new NonEmptyArraySeq(whole ++ as)

  def ++[B >: A](b: NonEmptyArraySeq[B]): NonEmptyArraySeq[B] =
    new NonEmptyArraySeq(whole ++ b.whole)

  def ++:[B >: A](as: ArraySeq[B]): NonEmptyArraySeq[B] =
    new NonEmptyArraySeq(as ++ whole)

  def reverse: NonEmptyArraySeq[A] =
    new NonEmptyArraySeq(whole.reverse)

  @inline def foldLeft[B](z: B)(f: (B, A) => B): B =
    whole.foldLeft(z)(f)

  def foldMapLeft1[B](g: A => B)(f: (B, A) => B): B = {
    var b = g(head)
    var i = 1
    while (i < whole.length) {
      val a = whole(i)
      b = f(b, a)
      i += 1
    }
    b
  }

  def reduceMapLeft1[B](f: A => B)(g: (B, B) => B): B =
    foldMapLeft1(f)((b, a) => g(b, f(a)))

  def reduce[B >: A](f: (B, B) => B): B =
    reduceMapLeft1[B](a => a)(f)

  def filter(f: A => Boolean): Option[NonEmptyArraySeq[A]] =
    NonEmptyArraySeq.option(whole filter f)

  def filterNot(f: A => Boolean): Option[NonEmptyArraySeq[A]] =
    filter(!f(_))

  def mapToNES[B: UnivEq](f: A => B): NonEmptySet[B] =
    NonEmptySet force iterator.map(f).toSet

  def toNES[B >: A : UnivEq]: NonEmptySet[B] =
    NonEmptySet(head, tail.toSet[B])

  def mapToNEV[B: UnivEq](f: A => B): NonEmptyVector[B] =
    NonEmptyVector force iterator.map(f).toVector

  def toNEV[B >: A : UnivEq]: NonEmptyVector[B] =
    NonEmptyVector(head, tail.toVector)

  private def safeTrans[B](f: ArraySeq[A] => ArraySeq[B]): NonEmptyArraySeq[B] =
    NonEmptyArraySeq force f(whole)

  def sorted[B >: A](implicit ord: Ordering[B])       = safeTrans(_.sorted[B])
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]) = safeTrans(_ sortBy f)
  def sortWith(lt: (A, A) => Boolean)                 = safeTrans(_ sortWith lt)

//  def partitionD[B, C](f: A => B \/ C): (NonEmptyArraySeq[B], ArraySeq[C]) \/ (ArraySeq[B], NonEmptyArraySeq[C]) = {
//    var bs = ArraySeq.empty[B]
//    var cs = ArraySeq.empty[C]
//    for (a <- tail)
//      f(a) match {
//        case -\/(b) => bs :+= b
//        case \/-(c) => cs :+= c
//      }
//    f(head) match {
//      case -\/(b) => -\/((NonEmptyArraySeq(b, bs), cs))
//      case \/-(c) => \/-((bs, NonEmptyArraySeq(c, cs)))
//    }
//  }
//
//  def partitionB(f: A => Boolean): (NonEmptyArraySeq[A], ArraySeq[A]) = {
//    var ts = ArraySeq.empty[A]
//    var fs = ArraySeq.empty[A]
//    for (a <- tail)
//      if (f(a))
//        ts :+= a
//      else
//        fs :+= a
//    if (ts.nonEmpty)
//      (NonEmptyArraySeq force ts, fs)
//    else
//      (NonEmptyArraySeq force fs, ts)
//  }

  /**
   * Peels away elements from the end until there are no elements left.
   *
   * Example:
   *
   * NonEmptyArraySeq(2,4,6,8) will yield
   *
   *   NonEmptyArraySeq(2,4,6,8)
   *   NonEmptyArraySeq(2,4,6)
   *   NonEmptyArraySeq(2,4)
   *   NonEmptyArraySeq(2)
   */
  def peelFromEnd: Iterator[NonEmptyArraySeq[A]] =
    new AbstractIterator[NonEmptyArraySeq[A]] {
      var cur: NonEmptyArraySeq[A] = NonEmptyArraySeq.this
      override def hasNext = cur ne null
      override def next() = {
        val r = cur
        cur = r.initNonEmpty.orNull
        r
      }
    }

  @inline def mkString(start: String, sep: String, end: String): String =
    whole.mkString(start, sep, end)

  @inline def mkString(sep: String): String =
    whole.mkString(sep)

  @inline def mkString: String =
    whole.mkString

  def to[B](factory: Factory[A, B]): B =
    factory.fromSpecific(whole)

  def traverse[G[_], B: ClassTag](f: A => G[B])(implicit ap: Apply[G]): G[NonEmptyArraySeq[B]] = {
    val gh = f(head)
    if (tail.isEmpty)
      ap.map(gh)(NonEmptyArraySeq.one)
    else {
      val gz = ap.map(gh)(_ => ArraySeq.empty[B])
      val gt = tail.foldLeft(gz)((q, a) => ap.apply2(q, f(a))(_ :+ _))
      ap.apply2(gh, gt)((h, t) => NonEmptyArraySeq.force(h +: t))
    }
  }

  def updated[AA >: A](idx: Int, value: AA): NonEmptyArraySeq[AA] =
    NonEmptyArraySeq.force(whole.updated(idx, value))
}

// =====================================================================================================================

object NonEmptyArraySeq extends NonEmptyArraySeqImplicits0 {

  def force[A](v: ArraySeq[A]): NonEmptyArraySeq[A] =
    new NonEmptyArraySeq(v)

  def apply[A: ClassTag](h: A, t: A*): NonEmptyArraySeq[A] =
    if (t.isEmpty)
      one(h)
    else {
      val b = ArraySeq.newBuilder[A]
      b += h
      b ++= t
      new NonEmptyArraySeq(b.result())
    }

  def one[A: ClassTag](h: A): NonEmptyArraySeq[A] = {
    val a = new Array[A](1)
    a(0) = h
    new NonEmptyArraySeq(ArraySeq.unsafeWrapArray(a))
  }

  def endOV[A: ClassTag](init: Option[ArraySeq[A]], last: A): NonEmptyArraySeq[A] =
    init.fold(one(last))(end(_, last))

  def endO[A: ClassTag](init: Option[NonEmptyArraySeq[A]], last: A): NonEmptyArraySeq[A] =
    init.fold(one(last))(_ :+ last)

  def end[A: ClassTag](init: ArraySeq[A], last: A): NonEmptyArraySeq[A] =
    if (init.isEmpty)
      one(last)
    else
      new NonEmptyArraySeq(init :+ last)

  def maybe[A, B](v: ArraySeq[A], empty: => B)(f: NonEmptyArraySeq[A] => B): B =
    if (v.isEmpty) empty else f(force(v))

  def option[A](v: ArraySeq[A]): Option[NonEmptyArraySeq[A]] =
    maybe[A, Option[NonEmptyArraySeq[A]]](v, None)(Some.apply)

  def fromNEV[A: ClassTag](nev: NonEmptyVector[A]): NonEmptyArraySeq[A] = {
    val as = new Array[A](nev.length)
    nev.foreachWithIndex((a, i) => as(i) = a)
    force(ArraySeq.unsafeWrapArray(as))
  }

  def split(string: String, regex: String): NonEmptyArraySeq[String] =
    force(ArraySeq.unsafeWrapArray(string.split(regex)))

  def split(string: String, char: Char): NonEmptyArraySeq[String] =
    force(ArraySeq.unsafeWrapArray(string.split(char)))

  def unwrapOption[A: ClassTag](o: Option[NonEmptyArraySeq[A]]): ArraySeq[A] =
    o.fold(ArraySeq.empty[A])(_.whole)

  def newBuilder[A: ClassTag](head: A): Builder[A] =
    new Builder(head)

  def newBuilderNE[A: ClassTag](as: NonEmptyArraySeq[A]): Builder[A] = {
    val b = newBuilder(as.head)
    b ++= as.tail
    b
  }

  final class Builder[A: ClassTag](head: A) {
    private[this] val whole = ArraySeq.newBuilder[A]
    whole += head

    def +=(a: A): Unit = {
      whole += a
      ()
    }

    def ++=(as: IterableOnce[A]): Unit = {
      whole ++= as
      ()
    }

    def +++=(as: NonEmptyArraySeq[A]): Unit =
      this ++= as.whole

    def result(): NonEmptyArraySeq[A] =
      NonEmptyArraySeq.force(whole.result())
  }

  implicit def univEq[A: UnivEq]: UnivEq[NonEmptyArraySeq[A]] =
    UnivEq.force

  implicit def proveNEA[A]: NonEmpty.Proof[ArraySeq[A], NonEmptyArraySeq[A]] =
    NonEmpty.Proof(option[A])

  implicit def semigroup[A]: Semigroup[NonEmptyArraySeq[A]] =
    new Semigroup[NonEmptyArraySeq[A]] {
      override def append(a: NonEmptyArraySeq[A], b: => NonEmptyArraySeq[A]): NonEmptyArraySeq[A] = a ++ b
    }

//  implicit def traverse1: Traverse1[NonEmptyArraySeq] = new Traverse1[NonEmptyArraySeq] {
//    override def foldLeft[A, B](fa: NonEmptyArraySeq[A], z: B)(f: (B, A) => B): B =
//      fa.foldLeft(z)(f)
//
//    override def foldMapRight1[A, B](fa: NonEmptyArraySeq[A])(z: A => B)(f: (A, => B) => B): B =
//      fa.init.reverseIterator.foldLeft(z(fa.last))((b, a) => f(a, b))
//
//    override def index[A](fa: NonEmptyArraySeq[A], i: Int): Option[A] =
//      fa(i)
//
//    override def length[A](fa: NonEmptyArraySeq[A]) =
//      fa.length
//
//    override def map[A, B](fa: NonEmptyArraySeq[A])(f: A => B): NonEmptyArraySeq[B] =
//      fa map f
//
//    override def traverse1Impl[G[_], A, B](fa: NonEmptyArraySeq[A])(f: A => G[B])(implicit ap: Apply[G]): G[NonEmptyArraySeq[B]] = {
//      val gh = f(fa.head)
//      if (fa.tail.isEmpty)
//        ap.map(gh)(one)
//      else {
//        val gz = ap.map(gh)(_ => ArraySeq.empty[B])
//        val gt = fa.tail.foldLeft(gz)((q, a) => ap.apply2(q, f(a))(_ :+ _))
//        ap.apply2(gh, gt)((h, t) => force(h +: t))
//      }
//    }
//  }

//  object Sole {
//    def unapply[A](v: NonEmptyArraySeq[A]) = new Unapply(v)
//    final class Unapply[A](val v: NonEmptyArraySeq[A]) extends AnyVal {
//      def isEmpty = v.tail.nonEmpty
//      def get     = v.head
//    }
//  }
}

trait NonEmptyArraySeqImplicits1 {
//  implicit def order[A: Order]: Order[NonEmptyArraySeq[A]] =
//    vectorOrder[A].contramap(_.whole)
}

trait NonEmptyArraySeqImplicits0 extends NonEmptyArraySeqImplicits1 {
//  implicit def equality[A: Equal]: Equal[NonEmptyArraySeq[A]] =
//    vectorEqual[A].contramap(_.whole)
}
