package shipreq.base.util

import japgolly.univeq.UnivEq

object Memo {
  // Because of annoying Intellij IDEA
  @inline private def platform: PlatformShared = shipreq.base.util.Platform

  @inline def apply[A: UnivEq, B](f: A => B): A => B =
    platform memo f

  def bool[A](f: Boolean => A): Boolean => A = {
    val t = f(true)
    val z = f(false)
    b => if (b) t else z
  }

  @inline def int[A](f: Int => A): Int => A =
    platform memoInt f

  @inline def fn0[A](a: => A): () => A =
    platform.memoFn0(() => a)

  def curry2[A: UnivEq, B: UnivEq, Z](f: A => B => Z): A => B => Z =
    Memo[A, B => Z](a => Memo(f(a)))

  def curry3[A: UnivEq, B: UnivEq, C: UnivEq, Z](f: A => B => C => Z): A => B => C => Z =
    Memo[A, B => C => Z](a => curry2(f(a)))

  def fn2[A: UnivEq, B: UnivEq, Z](f: (A, B) => Z): (A, B) => Z = {
    val m = Memo[(A, B), Z](f.tupled)
    (a, b) => m((a, b))
  }

  def by[I, K](k: I => K) = new By(k)

  final class By[I, K] private[Memo] (private val k: I => K) extends AnyVal {
    def apply[O](f: I => O)(implicit ev: UnivEq[K]): I => O = {
      val m = platform.looseMemo[K, O]()
      i => m(k(i), f(i))
    }
  }

  def byRef[A <: AnyRef, B](f: A => B): A => B =
    by[A, Ref[A]](Ref.apply)(f)
}

/*
// =====================================================================================================================
import shipreq.base.util.Platform._

case class Memoisable[A](memoise: A => A) extends AnyVal

object MemoisableFns {

  // -----------------------------------------------------------------------------------------------
  // Lowest pri here - checked last

  trait T01 {
    implicit def fn1[A: UnivEq, B] = Memoisable[A => B](memo)
  }

  trait T02 extends T01 {

    implicit def intFn[A] = Memoisable[Int => A](memoInt)

//    implicit def fn2[A: UnivEq, B: UnivEq, Z] = Memoisable[(A, B) => Z]{ f =>
//      val m = Memo(f.tupled)
//      (a, b) => m((a, b))
//    }
  }

  trait All extends T02 {
    implicit def curry[A, B](implicit ma: Memoisable[A => B], mb: Memoisable[B]) = Memoisable[A => B](f =>
      ma.memoise(a => mb.memoise(f(a))))
  }

  // Highest pri here - checked first
  // -----------------------------------------------------------------------------------------------
}

object Memoisable extends MemoisableFns.All

object Memo {

  def apply[A](a: A)(implicit m: Memoisable[A]): A =
    m memoise a

  def by[I, K: UnivEq, O](k: I => K)(f: I => O): I => O = {
    val m = looseMemo[K, O]()
    i => m(k(i), f(i))
  }
}
*/

/*
// =====================================================================================================================
import shipreq.base.util.Platform._

case class Memoisable[A](memoise: A => A) extends AnyVal

object MemoisableFns {

  // -----------------------------------------------------------------------------------------------
  // Lowest pri here - checked last

  trait T01 {
    implicit def fn1[A: UnivEq, B] = Memoisable[A => B](memo)
  }

  trait T02 extends T01 {
    implicit def fn2[A: UnivEq, B: UnivEq, Z] = Memoisable[(A, B) => Z]{ f =>
      val m = Memo(f.tupled)
      (a, b) => m((a, b))
    }
  }

  trait All extends T02 {
    implicit def intFn[A] = Memoisable[Int => A](memoInt)
  }

  // Highest pri here - checked first
  // -----------------------------------------------------------------------------------------------
}

object Memoisable extends MemoisableFns.All

object Memo {

  def apply[A](a: A)(implicit m: Memoisable[A]): A =
    m memoise a

  def curry2[A, B, Z](f: A => B => Z)(implicit ma: Memoisable[A => B => Z], mb: Memoisable[B => Z]): A => B => Z =
    ma.memoise(a => mb.memoise(f(a)))

  def curry3[A, B, C, Z](f: A => B => C => Z)(implicit ma: Memoisable[A => B => Z], mb: Memoisable[B => Z]): A => B => Z =
    ma.memoise(a => mb.memoise(f(a)))

  def by[I, K: UnivEq, O](k: I => K)(f: I => O): I => O = {
    val m = looseMemo[K, O]()
    i => m(k(i), f(i))
  }
}
*/