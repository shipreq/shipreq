package shipreq.webapp.shared.prop

import scalaz.Leibniz._

sealed trait Distinct[A] {
  def apply(r: RngGen[List[A]]): RngGen[List[A]]
  final def +(d: Distinct[A]): Distinct[A] = DistinctN(r => d(apply(r)))
}

final case class DistinctN[A](f: RngGen[List[A]] => RngGen[List[A]]) extends Distinct[A] {
  override def apply(r: RngGen[List[A]]) = f(r)
}

final case class Distinct1[A, B](f: A => TraversableOnce[B], g: (A, Set[B]) => A, i: TraversableOnce[B]) extends Distinct[A] {
  override def apply(r: RngGen[List[A]]) = Distinct.applyF(r, f, g, i)

  def blacklist(j: TraversableOnce[B]) = copy[A, B](i = j)

  def contramap[Z](m: Z => A, n: (Z, A) => Z) =
    Distinct1[Z, B](f compose m, (z,bs) => n(z, g(m(z),bs)), i)
}

object Distinct {
  def on[A] = new B1[A]
  lazy val str  = on[String](identity).str ((_, b) => b)
  lazy val int  = on[Int]   (identity).int ((_, b) => b)
  lazy val long = on[Long]  (identity).long((_, b) => b)

  final class B1[A] {
    def apply[B](f: A => B)              = n[B](a => Set(f(a)))
    def n[B](f: A => TraversableOnce[B]) = new B2[A, B](f)
    def self = apply(identity)
  }

  final class B2[A, B](f: A => TraversableOnce[B]) {
    def apply(g: (A, Set[B]) => A) = Distinct1(f, g, Set.empty)
    def str (g: (A, String) => A)(implicit ev: B === String) = apply((a, bs) => g(a, distinctStr(ev subst bs)))
    def int (g: (A, Int)    => A)(implicit ev: B === Int)    = apply((a, bs) => g(a, (ev subst bs).max + 1))
    def long(g: (A, Long)   => A)(implicit ev: B === Long)   = apply((a, bs) => g(a, (ev subst bs).max + 1L))
  }

  def distinctStr(bs: Set[String]): String = {
    val x = bs.max
    if (x.nonEmpty) {
      val c = x.head
      if (c < 0xffff) return (c + 1).toChar.toString
    }
    val y = bs.min
    if (y.nonEmpty) {
      val c = y.head
      if (c > 32) return (c - 1).toChar.toString
    }
    "\uffff" + x
  }

  def applyF[A, B](r: RngGen[List[A]], f: A => TraversableOnce[B], g: (A, Set[B]) => A, ib: TraversableOnce[B]): RngGen[List[A]] =
    r.flatMap(apply(_, f, g, ib))

  def apply[A, B](as: List[A], f: A => TraversableOnce[B], g: (A, Set[B]) => A, ib: TraversableOnce[B]): RngGen[List[A]] = {
    var bs = ib.toSet
    var ok = List.empty[A]
    var ko = List.empty[A]
    as.foreach(a => {
      val bs2 = f(a).toSet
      val dup = bs.exists(bs2.contains)
      if (dup)
        ko = a :: ko
      else {
        bs ++= bs2
        ok = a :: ok
      }
    })
    if (ko.isEmpty)
      Gen.insert(as)
    else
      ko.foldLeft(Gen.insert(ok, bs))(
        (q, a) => q.map { case (as, bs) =>
          val a2 = g(a, bs)
          val bs2 = f(a2).toSet
          if (bs.exists(bs2.contains)) {
            println(s"${"-" * 100}\nA.old: $a\nA.new: $a2\nBs.A: $bs2\nBs.Old: $bs")
            throw new java.lang.AssertionError(s"Data still not distinct.")
          }
          (a2 :: as, bs ++ bs2)
        }
      ).map(_._1)
  }
}