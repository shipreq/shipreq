package shipreq.webapp.base.prop

import scalaz.{NonEmptyList, Foldable, State}
import scalaz.Leibniz.===
import scalaz.syntax.foldable._
import shipreq.base.util.Baggy, Baggy._
import Distinct.Fixer

sealed trait DistinctFn[A, B] {
  def run: A => B
}

case class Distinct[A, X, H[_] : Baggy, Y, Z, B](
    fixer: Fixer[X, H, Y, Z], t: A => (X => State[H[Y], Z]) => State[H[Y], B]) extends DistinctFn[A, B]{

  final type S[λ] = State[H[Y], λ]

  def runs(a: A): S[B] =
    t(a)(fixer.apply)

  def run: A => B =
    runs(_).eval(fixer.inith)

  def addh(xs: X*) =
    copy(fixer = this.fixer.addh(xs: _*))

  @inline final def contramap[C](f: C => A, g: (C, B) => C) =
    dimap(f, g)

  def dimap[M, N](f: M => A, g: (M, B) => N) =
    Distinct[M, X, H, Y, Z, N](fixer, m => x_sz => t(f(m))(x_sz).map(b => g(m, b)))

  def dimaps[M, N](f: M => (A => S[B]) => S[N]) =
    Distinct[M, X, H, Y, Z, N](fixer, m => x_sz => f(m)(a => t(a)(x_sz)))

  def lift[F[_] : Foldable : Baggy]: Distinct[F[A], X, H, Y, Z, F[B]] =
    dimaps[F[A], F[B]](fa => ab => State(h0 =>
      fa.foldl((h0, implicitly[Baggy[F]].empty[B]))(q => a => {
        val (h1, fb) = q
        val (h2, b) = ab(a).run(h1)
        (h2, fb + b)
      })
    ))

  def compose[C](f: Distinct[C, X, H, Y, Z, A]) = f + this

  def +[C](f: Distinct[B, X, H, Y, Z, C]) =
    Distinct[A, X, H, Y, Z, C](fixer + f.fixer, a => _ => runs(a) flatMap f.runs)

  def *(f: DistinctFn[A, A])(implicit ev: B === A): DistinctEndo[A] =
    DistinctEndo(NonEmptyList(ev.subst[({type λ[α] = Distinct[A, X, H, Y, Z, α]})#λ](this), f))

  // def ***[C, D](f: Distinct1[C, D]): Distinct1[(A, C), (B, D)] =
}

case class DistinctEndo[A](ds: NonEmptyList[DistinctFn[A, A]]) extends DistinctFn[A, A] {
  def run: A => A =
    ds.tail.foldLeft(ds.head.run)(_ compose _.run)

  def *(d: DistinctFn[A, A]): DistinctEndo[A] =
    DistinctEndo(d <:: ds)

  def map[B](f: DistinctFn[A, A] => DistinctFn[B, B]): DistinctEndo[B] =
    DistinctEndo(ds map f)

  def contramap[B](f: B => A, g: (B, A) => B): DistinctEndo[B] = map {
    case d@DistinctEndo(_) => d.contramap(f, g)
    case d@Distinct(_, _)  => d.contramap(f, g)
  }

  def lift[F[_] : Foldable : Baggy]: DistinctEndo[F[A]] = map {
    case d@DistinctEndo(_) => d.lift[F]
    case d@Distinct(_, _)  => d.lift[F]
  }
}

// =====================================================================================================================

object Distinct {

  case class Fixer[X, H[_] : Baggy, Y, Z](f: X => Y, g: Y => Z, fix: H[Y] => Y, inith: H[Y]) {
    def apply(x: X): State[H[Y], Z] =
      State[H[Y], Z](h => {
        var y = f(x)
        if (h contains y)
          y = fix(h)
        (h + y, g(y))
      })

    @inline final def xmap[A](b: Z => A)(a: A => X) =
      dimap(a, b)

    def dimap[A, B](a: A => X, b: Z => B) =
      Fixer[A, H, Y, B](f compose a, b compose g, fix, inith)

    def addh(xs: X*) =
      copy[X, H, Y, Z](inith = xs.foldLeft(this.inith)(_ + f(_)))

    def +(φ: Fixer[X, H, Y, Z]) =
      copy[X, H, Y, Z](inith = this.inith ++ φ.inith)

    def distinct =
      Distinct[X, X, H, Y, Z, Z](this, x => f => f(x))
  }

  object Fixer {
    def lift[H[_], A](f: H[A] => A)(implicit H: Baggy[H]) =
      Fixer[A, H, A, A](identity, identity, f, H.empty)
  }

  // =====================================================================================================================

  def fixInt(is: Set[Int]): Int = {
    var i = is.max + 1
    if (i == Int.MinValue) while (is contains i) i += 1
    i
  }

  def fixLong(is: Set[Long]): Long = {
    var i = is.max + 1L
    if (i == Long.MinValue) while (is contains i) i += 1L
    i
  }

  def fixStr(ss: Set[String]): String = {
    val x = ss.max
    if (x.nonEmpty) {
      val c = x.head
      if (c < 0xffff) return (c + 1).toChar.toString
    }
    val y = ss.min
    if (y.nonEmpty) {
      val c = y.head
      if (c > 32) return (c - 1).toChar.toString
    }
    "\uffff" + x
  }

  lazy val fstr  = Fixer lift fixStr
  lazy val fint  = Fixer lift fixInt
  lazy val flong = Fixer lift fixLong

  lazy val str  = fstr.distinct
  lazy val int  = fint.distinct
  lazy val long = flong.distinct
}