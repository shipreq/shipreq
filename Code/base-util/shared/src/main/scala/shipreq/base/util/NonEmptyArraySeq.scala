package shipreq.base.util

import japgolly.microlibs.nonempty.NonEmpty
import japgolly.univeq.UnivEq
import scala.collection.{AbstractIterator, Factory}
import scala.collection.immutable.{ArraySeq, Range}
import scala.math.Ordering
import scala.reflect.ClassTag

final class NonEmptyArraySeq[+A](val head: A, val tail: ArraySeq[A]) {
  override def toString = "NonEmpty" + whole.toString

  override def hashCode = head.## * 31 + tail.##

  override def equals(o: Any) = o match {
    case that: NonEmptyArraySeq[Any] => this.head == that.head && this.tail == that.tail
    case _ => false
  }

  def length: Int =
    tail.length + 1

  def unsafeApply(i: Int): A =
    if (i == 0)
      head
    else
      tail(i - 1)

  def apply(i: Int): Option[A] =
    try {
      Some(unsafeApply(i))
    } catch {
      case _: IndexOutOfBoundsException => None
    }

  def init: ArraySeq[A] =
    if (tail.isEmpty)
      ArraySeq.empty
    else
      head +: tail.init

  def initNonEmpty: Option[NonEmptyArraySeq[A]] = NonEmptyArraySeq option init
  def tailNonEmpty: Option[NonEmptyArraySeq[A]] = NonEmptyArraySeq option tail

  def map[B](f: A => B): NonEmptyArraySeq[B] =
    NonEmptyArraySeq(f(head), tail map f)

  def flatMap[B](f: A => NonEmptyArraySeq[B]): NonEmptyArraySeq[B] =
    reduceMapLeft1(f)(_ ++ _)

  def foreach[U](f: A => U): Unit = {
    f(head)
    tail foreach f
  }

  def foreachWithIndex[U](f: (A, Int) => U): Unit = {
    f(head, 0)
    var i = 0
    for (a <- tail) {
      i += 1
      f(a, i)
    }
  }

  def indices: Range =
    0 until length

  def forall(f: A => Boolean): Boolean =
    f(head) && tail.forall(f)

  def exists(f: A => Boolean): Boolean =
    f(head) || tail.exists(f)

  def find(f: A => Boolean): Option[A] =
    if (f(head)) Some(head) else tail.find(f)

  def mapTail[B >: A](f: ArraySeq[A] => ArraySeq[B]): NonEmptyArraySeq[B] =
    NonEmptyArraySeq(head, f(tail))

  def mapWithIndex[B: ClassTag](f: (A, Int) => B): NonEmptyArraySeq[B] = {
    val h = f(head, 0)
    var i = 0
    var t = ArraySeq.empty[B]
    for (a <- tail) {
      i += 1
      t :+= f(a, i)
    }
    NonEmptyArraySeq(h, t)
  }

  def :+[B >: A](a: B): NonEmptyArraySeq[B] =
    mapTail(_ :+ a)

  def +:[B >: A](a: B): NonEmptyArraySeq[B] =
    NonEmptyArraySeq(a, head +: tail)

  def ++[B >: A](as: IterableOnce[B]): NonEmptyArraySeq[B] =
    mapTail(_ ++ as)

  def ++[B >: A](b: NonEmptyArraySeq[B]): NonEmptyArraySeq[B] =
    ++(b.whole)

  def ++:[B >: A](as: ArraySeq[B]): NonEmptyArraySeq[B] =
    if (as.isEmpty) this else NonEmptyArraySeq(as.head, as.tail ++ whole)

  def last: A =
    if (tail.isEmpty) head else tail.last

  lazy val whole: ArraySeq[A] =
    head +: tail

  def reverse: NonEmptyArraySeq[A] =
    if (tail.isEmpty) this else NonEmptyArraySeq.end(tail.reverse, head)

  def foldLeft[B](z: B)(f: (B, A) => B): B =
    tail.foldLeft(f(z, head))(f)

  def foldMapLeft1[B](g: A => B)(f: (B, A) => B): B =
    tail.foldLeft(g(head))(f)

  def reduceMapLeft1[B](f: A => B)(g: (B, B) => B): B =
    foldMapLeft1(f)((b, a) => g(b, f(a)))

  def reduce[B >: A](f: (B, B) => B): B =
    reduceMapLeft1[B](a => a)(f)

  // Reduce bullshit red in IntelliJ
//  def traverseD[L, B](f: A => L \/ B): L \/ NonEmptyArraySeq[B] =
//    NonEmptyArraySeq.traverse1.traverseU(this)(f)

//  def intercalate[B >: A](b: B): NonEmptyArraySeq[B] =
//    intercalateF(b)(a => a)
//
//  def intercalateF[B](b: B)(f: A => B): NonEmptyArraySeq[B] = {
//    val r = implicitly[Factory[B, ArraySeq[B]]].newBuilder
//    for (a <- tail) {
//      r += b
//      r += f(a)
//    }
//    NonEmptyArraySeq(f(head), r.result())
//  }

  def filter(f: A => Boolean): Option[NonEmptyArraySeq[A]] =
    NonEmptyArraySeq.option(whole filter f)

  def filterNot(f: A => Boolean): Option[NonEmptyArraySeq[A]] =
    filter(!f(_))

  def iterator: Iterator[A] =
    whole.iterator

//  def mapToNES[B: UnivEq](f: A => B): NonEmptySet[B] =
//    NonEmptySet force iterator.map(f).toSet
//
//  def toNES[B >: A : UnivEq]: NonEmptySet[B] =
//    NonEmptySet(head, tail.toSet[B])

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

  def mkString(start: String, sep: String, end: String): String =
    whole.mkString(start, sep, end)

  def mkString(sep: String): String = mkString("", sep, "")

  def mkString: String = mkString("")

  def to[B](factory: Factory[A, B]): B =
    factory.fromSpecific(whole)
}

// =====================================================================================================================

object NonEmptyArraySeq extends NonEmptyArraySeqImplicits0 {
  def one[A](h: A): NonEmptyArraySeq[A] =
    new NonEmptyArraySeq(h, ArraySeq.empty)

//  /** Avoids failed type-inference with NonEmptyArraySeq(ArraySeq.empty[Int], ArraySeq.empty[Int]) */
//  def varargs[A](h: A, t: A*): NonEmptyArraySeq[A] =
//    apply(h, ArraySeq.unsafeWrapArray())
//
//  def apply[A](h: A, t: A*): NonEmptyArraySeq[A] =
//    apply(h, t.toArraySeq)
//
  def apply[A](h: A, t: ArraySeq[A]): NonEmptyArraySeq[A] =
    new NonEmptyArraySeq(h, t)

  def endOV[A](init: Option[ArraySeq[A]], last: A): NonEmptyArraySeq[A] =
    init.fold(one(last))(end(_, last))

  def endO[A](init: Option[NonEmptyArraySeq[A]], last: A): NonEmptyArraySeq[A] =
    init.fold(one(last))(_ :+ last)

  def end[A](init: ArraySeq[A], last: A): NonEmptyArraySeq[A] =
    if (init.isEmpty)
      one(last)
    else
      new NonEmptyArraySeq(init.head, init.tail :+ last)

  def maybe[A, B](v: ArraySeq[A], empty: => B)(f: NonEmptyArraySeq[A] => B): B =
    if (v.isEmpty) empty else f(NonEmptyArraySeq(v.head, v.tail))

  def option[A](v: ArraySeq[A]): Option[NonEmptyArraySeq[A]] =
    maybe[A, Option[NonEmptyArraySeq[A]]](v, None)(Some.apply)

  def force[A](v: ArraySeq[A]): NonEmptyArraySeq[A] =
    apply(v.head, v.tail)

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
    private[this] val tail = ArraySeq.newBuilder[A]

    def +=(a: A): Unit = {
      tail += a
      ()
    }

    def ++=(as: IterableOnce[A]): Unit = {
      tail ++= as
      ()
    }

    def ++=(as: NonEmptyArraySeq[A]): Unit = {
      this += as.head
      this ++= as.tail
    }

    def result(): NonEmptyArraySeq[A] =
      NonEmptyArraySeq(head, tail.result())
  }

  implicit def univEq[A: UnivEq]: UnivEq[NonEmptyArraySeq[A]] =
    UnivEq.force

  implicit def proveNEA[A]: NonEmpty.Proof[ArraySeq[A], NonEmptyArraySeq[A]] =
    NonEmpty.Proof(option[A])

//  implicit def semigroup[A]: Semigroup[NonEmptyArraySeq[A]] =
//    new Semigroup[NonEmptyArraySeq[A]] {
//      override def append(a: NonEmptyArraySeq[A], b: => NonEmptyArraySeq[A]): NonEmptyArraySeq[A] = a ++ b
//    }
//
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
//        ap.apply2(gh, gt)(new NonEmptyArraySeq(_, _))
//      }
//    }
//  }
//
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
