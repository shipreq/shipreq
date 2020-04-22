package shipreq.base.util.storecache

import japgolly.univeq.UnivEq
import scala.annotation.tailrec

/** Quick equality check. */
final case class QuickEq[A](areEq: (A, A) => Boolean) extends AnyVal {

  def contramap[B](f: B => A): QuickEq[B] =
    QuickEq[B]((x, y) => areEq(f(x), f(y)))

  def narrow[B <: A]: QuickEq[B] =
    new QuickEq[B](areEq)

  def &&(next: QuickEq[A]): QuickEq[A] =
    QuickEq((x, y) => areEq(x, y) && next.areEq(x, y))

  def ||(next: QuickEq[A]): QuickEq[A] =
    QuickEq((x, y) => areEq(x, y) || next.areEq(x, y))

  def tuple[B](b: QuickEq[B]): QuickEq[(A, B)] =
    QuickEq((x, y) => areEq(x._1, y._1) && b.areEq(x._2, y._2))
}

object QuickEq {

  def byRef[A <: AnyRef]: QuickEq[A] =
    apply(_ eq _)

  def byRefOrUnivEq[A <: AnyRef: UnivEq]: QuickEq[A] =
    apply((x, y) => (x eq y) || (x == y))

  def byUnivEq[A: UnivEq]: QuickEq[A] =
    apply(_ == _)

  def always[A]: QuickEq[A] =
    apply((_, _) => true)

  def never[A]: QuickEq[A] =
    apply((_, _) => false)

  def const[A](r: Boolean): QuickEq[A] =
    if (r) always else never

  def by[A, B](f: A => B)(implicit r: QuickEq[B]): QuickEq[A] =
    r contramap f

  def byIterator[I[X] <: Iterable[X], A: QuickEq]: QuickEq[I[A]] =
    apply { (x, y) =>
      val i = x.iterator
      val j = y.iterator
      @tailrec
      def go: Boolean = {
        val hasNext = i.hasNext
        if (hasNext != j.hasNext)
          false
        else if (!hasNext)
          true
        else if (i.next() ~/~ j.next())
          false
        else
          go
      }
      go
    }

  def indexedSeq[S[X] <: IndexedSeq[X], A: QuickEq]: QuickEq[S[A]] =
    apply((x, y) =>
      (x.length == y.length) && x.indices.forall(i => x(i) ~=~ y(i)))

  /** Declare a type reusable when both values pass a given predicate. */
  def when[A](f: A => Boolean): QuickEq[A] =
    apply((a, b) => f(a) && f(b))

  /** Declare a type reusable when both values fail a given predicate. */
  def unless[A](f: A => Boolean): QuickEq[A] =
    when(!f(_))

  def embedded[A, B](s: A => QuickEq[B])(b: A => B): QuickEq[A] =
    QuickEq((x, y) => {
      val s1 = s(x)
      val s2 = s(y)
      val b1 = b(x)
      val b2 = b(y)
      s1.areEq(b1, b2) && s2.areEq(b1, b2)
    })

  def list[A: QuickEq]: QuickEq[List[A]] =
    byRef[List[A]] || byIterator[List, A]

  def vector[A: QuickEq]: QuickEq[Vector[A]] =
    byRef[Vector[A]] || indexedSeq[Vector, A]

  def set[A: UnivEq]: QuickEq[Set[A]] =
    byRefOrUnivEq

  @inline private implicit final class Ext_Any[A](private val self: A) extends AnyVal {
    @inline def ~=~(a: A)(implicit r: QuickEq[A]): Boolean = r.areEq(self, a)
    @inline def ~/~(a: A)(implicit r: QuickEq[A]): Boolean = !r.areEq(self, a)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Implicit Instances

  @inline implicit def unit   : QuickEq[Unit   ] = always
  @inline implicit def boolean: QuickEq[Boolean] = byUnivEq
  @inline implicit def byte   : QuickEq[Byte   ] = byUnivEq
  @inline implicit def char   : QuickEq[Char   ] = byUnivEq
  @inline implicit def short  : QuickEq[Short  ] = byUnivEq
  @inline implicit def int    : QuickEq[Int    ] = byUnivEq
  @inline implicit def long   : QuickEq[Long   ] = byUnivEq
  @inline implicit def string : QuickEq[String ] = byUnivEq


  implicit def option[A: QuickEq]: QuickEq[Option[A]] =
    apply((x, y) =>
      x.fold(y.isEmpty)(xa =>
        y.fold(false)(ya =>
          xa ~=~ ya)))

  implicit def either[A: QuickEq, B: QuickEq]: QuickEq[Either[A, B]] =
    apply((x, y) =>
      x.fold[Boolean](
        a => y.fold(a ~=~ _, _ => false),
        b => y.fold(_ => false, b ~=~ _)))

  implicit def tuple2[A:QuickEq, B:QuickEq]: QuickEq[(A,B)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2))

  implicit def tuple3[A:QuickEq, B:QuickEq, C:QuickEq]: QuickEq[(A,B,C)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3))

  implicit def tuple4[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq]: QuickEq[(A,B,C,D)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4))

  implicit def tuple5[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq]: QuickEq[(A,B,C,D,E)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5))

  implicit def tuple6[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq]: QuickEq[(A,B,C,D,E,F)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6))

  implicit def tuple7[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq]: QuickEq[(A,B,C,D,E,F,G)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7))

  implicit def tuple8[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8))

  implicit def tuple9[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9))

  implicit def tuple10[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq, J:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I,J)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9) && (x._10 ~=~ y._10))

  implicit def tuple11[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq, J:QuickEq, K:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I,J,K)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9) && (x._10 ~=~ y._10) && (x._11 ~=~ y._11))

  implicit def tuple12[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq, J:QuickEq, K:QuickEq, L:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I,J,K,L)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9) && (x._10 ~=~ y._10) && (x._11 ~=~ y._11) && (x._12 ~=~ y._12))

  implicit def tuple13[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq, J:QuickEq, K:QuickEq, L:QuickEq, M:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I,J,K,L,M)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9) && (x._10 ~=~ y._10) && (x._11 ~=~ y._11) && (x._12 ~=~ y._12) && (x._13 ~=~ y._13))

  implicit def tuple14[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq, J:QuickEq, K:QuickEq, L:QuickEq, M:QuickEq, N:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I,J,K,L,M,N)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9) && (x._10 ~=~ y._10) && (x._11 ~=~ y._11) && (x._12 ~=~ y._12) && (x._13 ~=~ y._13) && (x._14 ~=~ y._14))

  implicit def tuple15[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq, J:QuickEq, K:QuickEq, L:QuickEq, M:QuickEq, N:QuickEq, O:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9) && (x._10 ~=~ y._10) && (x._11 ~=~ y._11) && (x._12 ~=~ y._12) && (x._13 ~=~ y._13) && (x._14 ~=~ y._14) && (x._15 ~=~ y._15))

  implicit def tuple16[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq, J:QuickEq, K:QuickEq, L:QuickEq, M:QuickEq, N:QuickEq, O:QuickEq, P:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9) && (x._10 ~=~ y._10) && (x._11 ~=~ y._11) && (x._12 ~=~ y._12) && (x._13 ~=~ y._13) && (x._14 ~=~ y._14) && (x._15 ~=~ y._15) && (x._16 ~=~ y._16))

  implicit def tuple17[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq, J:QuickEq, K:QuickEq, L:QuickEq, M:QuickEq, N:QuickEq, O:QuickEq, P:QuickEq, Q:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9) && (x._10 ~=~ y._10) && (x._11 ~=~ y._11) && (x._12 ~=~ y._12) && (x._13 ~=~ y._13) && (x._14 ~=~ y._14) && (x._15 ~=~ y._15) && (x._16 ~=~ y._16) && (x._17 ~=~ y._17))

  implicit def tuple18[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq, J:QuickEq, K:QuickEq, L:QuickEq, M:QuickEq, N:QuickEq, O:QuickEq, P:QuickEq, Q:QuickEq, R:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9) && (x._10 ~=~ y._10) && (x._11 ~=~ y._11) && (x._12 ~=~ y._12) && (x._13 ~=~ y._13) && (x._14 ~=~ y._14) && (x._15 ~=~ y._15) && (x._16 ~=~ y._16) && (x._17 ~=~ y._17) && (x._18 ~=~ y._18))

  implicit def tuple19[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq, J:QuickEq, K:QuickEq, L:QuickEq, M:QuickEq, N:QuickEq, O:QuickEq, P:QuickEq, Q:QuickEq, R:QuickEq, S:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9) && (x._10 ~=~ y._10) && (x._11 ~=~ y._11) && (x._12 ~=~ y._12) && (x._13 ~=~ y._13) && (x._14 ~=~ y._14) && (x._15 ~=~ y._15) && (x._16 ~=~ y._16) && (x._17 ~=~ y._17) && (x._18 ~=~ y._18) && (x._19 ~=~ y._19))

  implicit def tuple20[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq, J:QuickEq, K:QuickEq, L:QuickEq, M:QuickEq, N:QuickEq, O:QuickEq, P:QuickEq, Q:QuickEq, R:QuickEq, S:QuickEq, T:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9) && (x._10 ~=~ y._10) && (x._11 ~=~ y._11) && (x._12 ~=~ y._12) && (x._13 ~=~ y._13) && (x._14 ~=~ y._14) && (x._15 ~=~ y._15) && (x._16 ~=~ y._16) && (x._17 ~=~ y._17) && (x._18 ~=~ y._18) && (x._19 ~=~ y._19) && (x._20 ~=~ y._20))

  implicit def tuple21[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq, J:QuickEq, K:QuickEq, L:QuickEq, M:QuickEq, N:QuickEq, O:QuickEq, P:QuickEq, Q:QuickEq, R:QuickEq, S:QuickEq, T:QuickEq, U:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9) && (x._10 ~=~ y._10) && (x._11 ~=~ y._11) && (x._12 ~=~ y._12) && (x._13 ~=~ y._13) && (x._14 ~=~ y._14) && (x._15 ~=~ y._15) && (x._16 ~=~ y._16) && (x._17 ~=~ y._17) && (x._18 ~=~ y._18) && (x._19 ~=~ y._19) && (x._20 ~=~ y._20) && (x._21 ~=~ y._21))

  implicit def tuple22[A:QuickEq, B:QuickEq, C:QuickEq, D:QuickEq, E:QuickEq, F:QuickEq, G:QuickEq, H:QuickEq, I:QuickEq, J:QuickEq, K:QuickEq, L:QuickEq, M:QuickEq, N:QuickEq, O:QuickEq, P:QuickEq, Q:QuickEq, R:QuickEq, S:QuickEq, T:QuickEq, U:QuickEq, V:QuickEq]: QuickEq[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)] =
    apply((x,y) => (x._1 ~=~ y._1) && (x._2 ~=~ y._2) && (x._3 ~=~ y._3) && (x._4 ~=~ y._4) && (x._5 ~=~ y._5) && (x._6 ~=~ y._6) && (x._7 ~=~ y._7) && (x._8 ~=~ y._8) && (x._9 ~=~ y._9) && (x._10 ~=~ y._10) && (x._11 ~=~ y._11) && (x._12 ~=~ y._12) && (x._13 ~=~ y._13) && (x._14 ~=~ y._14) && (x._15 ~=~ y._15) && (x._16 ~=~ y._16) && (x._17 ~=~ y._17) && (x._18 ~=~ y._18) && (x._19 ~=~ y._19) && (x._20 ~=~ y._20) && (x._21 ~=~ y._21) && (x._22 ~=~ y._22))
}

