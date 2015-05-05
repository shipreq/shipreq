package shipreq.webapp.client.util

import japgolly.scalajs.react.{BackendScope, ReactComponentB}
import japgolly.scalajs.react.ScalazReact._
import scala.runtime.AbstractFunction1
import scalaz.effect.IO
import shipreq.base.util.UnivEq
import shipreq.webapp.base.data.Project

/**
 * Wrapper denoting that the developer expects that the underlying value will never (or only rarely) change.
 *
 * Reference equality of the underlying value will be used, meaning that `A` need not have its own `Reusability` typeclass.
 */
final case class ReusableVal[A <: AnyRef](value: A)

object ReusableVal {

  @inline implicit def reusability[A <: AnyRef]: Reusable[ReusableVal[A]] =
    Reusable.byRef[A].contramap(_.value)

  @inline implicit def autoValue[A <: AnyRef](r: ReusableVal[A]): A =
    r.value
}

final class Reusable[A](val reusable: (A, A) => Boolean) extends AnyVal {
  def contramap[B](f: B => A): Reusable[B] =
    Reusable((x, y) => reusable(f(x), f(y)))
}

object Reusable {
  def apply[A](f: (A, A) => Boolean): Reusable[A] =
    new Reusable(f)

  def const[A](r: Boolean): Reusable[A] =
    new Reusable((_, _) => r)

  def byUnivEq[A: UnivEq]: Reusable[A] =
    new Reusable((a, b) => a == b)

  def byRef[A <: AnyRef]: Reusable[A] =
    new Reusable((a, b) => a eq b)

  def byRefThenUnivEq[A <: AnyRef: UnivEq]: Reusable[A] =
    new Reusable((a, b) => (a eq b) || (a == b))

  def by[A, B](f: A => B)(implicit r: Reusable[B]): Reusable[A] =
    r contramap f

  implicit val reusableInt    : Reusable[Int]     = byUnivEq
  implicit val reusableLong   : Reusable[Long]    = byUnivEq
  implicit val reusableString : Reusable[String]  = byUnivEq
  implicit val reusableProject: Reusable[Project] = Reusable.by((_: Project).rev.value)

  implicit def option[A: Reusable]: Reusable[Option[A]] =
    apply((x, y) => x.fold(y.isEmpty)(a => y.fold(false)(a ~=~ _)))

  def caseclass2[A: Reusable, B: Reusable, Z](f: Z => Option[(A, B)]): Reusable[Z] =
    apply { (x, y) =>
      val (xa,xb) = f(x).get
      val (ya,yb) = f(y).get
      (xa ~=~ ya) && (xb ~=~ yb)
    }

  def caseclass3[A: Reusable, B: Reusable, C: Reusable, Z](f: Z => Option[(A, B, C)]): Reusable[Z] =
    apply { (x, y) =>
      val (xa,xb,xc) = f(x).get
      val (ya,yb,yc) = f(y).get
      (xa ~=~ ya) && (xb ~=~ yb) && (xc ~=~ yc)
    }

  def caseclass4[A: Reusable, B: Reusable, C: Reusable, D: Reusable, Z](f: Z => Option[(A, B, C, D)]): Reusable[Z] =
    apply { (x, y) =>
      val (xa,xb,xc,xd) = f(x).get
      val (ya,yb,yc,yd) = f(y).get
      (xa ~=~ ya) && (xb ~=~ yb) && (xc ~=~ yc) && (xd ~=~ yd)
    }

  def caseclass5[A: Reusable, B: Reusable, C: Reusable, D: Reusable, E: Reusable, Z](f: Z => Option[(A, B, C, D, E)]): Reusable[Z] =
    apply { (x, y) =>
      val (xa,xb,xc,xd,xe) = f(x).get
      val (ya,yb,yc,yd,ye) = f(y).get
      (xa ~=~ ya) && (xb ~=~ yb) && (xc ~=~ yc) && (xd ~=~ yd) && (xe ~=~ ye)
    }

  def preventUpdates[P, B](implicit r: Reusable[P]) =
    (_: ReactComponentB[P, Unit, B]).shouldComponentUpdate(($, b, _) => $.props ~/~ b)

  def preventUpdatesDebug[P, B](name: String)(implicit r: Reusable[P]) =
    (_: ReactComponentB[P, Unit, B]).shouldComponentUpdate(($, b, _) => {
      val su = $.props ~/~ b
      println(s"$name.shouldComponentUpdate = $su")
      su
    })
}

// =====================================================================================================================


case class ReusableExternalVar[A](value: A, set: A ~=> IO[Unit])(implicit val reuse: Reusable[A]) {
  import monocle._

  def mod(f: A => A): IO[Unit] =
    set(f(value))

  def setL[B](l: Lens[A, B]): B => IO[Unit] =
    b => set(l.set(b)(value))

  def modL[B](l: Lens[A, B])(f: B => B): IO[Unit] =
    set(l.modify(f)(value))
}

object ReusableExternalVar {
  implicit def reusability[A]: Reusable[ReusableExternalVar[A]] =
    Reusable((a, b) => (a.set ~=~ b.set) && a.reuse.reusable(a.value, b.value))
}

// =====================================================================================================================

trait ReusableFn[A, B] extends AbstractFunction1[A, B] {
  def reusable: PartialFunction[ReusableFn[A, B], Boolean]

  import scalaz.Leibniz._

  def extvar(value: A)(implicit r: Reusable[A], ev: B === IO[Unit]): ReusableExternalVar[A] =
    ReusableExternalVar(value, ev.subst[({type λ[a] = ReusableFn[A, a]})#λ](this))(r)

  def extvarR(value: A, r: Reusable[A])(implicit ev: B === IO[Unit]): ReusableExternalVar[A] =
    extvar(value)(r, ev)
}

object ReusableFn {
  def apply[A, O](f: A => O): Fn1[A, O] = new Fn1(f)

  def apply[A: Reusable, B, O](f: (A, B) => O): Fn2[A, B, O] = new Fn2(f)

  def modStateIO[S, A]($: BackendScope[_, S])(f: S => A => S): A ~=> IO[Unit] =
    ReusableFn(a => $.modStateIO(s => f(s)(a)))

  implicit def anyFn[A, B]: Reusable[ReusableFn[A, B]] =
    Reusable((x, y) => x.reusable.applyOrElse(y, (_: ReusableFn[A, B]) => false))

  final class Fn1[A, O](val f: A => O) extends ReusableFn[A, O] {
    override def apply(a: A) = f(a)
    override def reusable = { case x: Fn1[A, O] => f eq x.f }
  }

  final class Fn2[A: Reusable, B, O](val f: (A, B) => O) extends ReusableFn[A, Cur1[A, B, O]] {
    override def apply(a: A) = new Cur1(a, f)
    override def reusable = { case x: Fn2[A, B, O] => f eq x.f }
  }

  final class Fn3[A: Reusable, B: Reusable, C, O](val f: (A, B, C) => O) extends ReusableFn[A, Cur1[A, B, Cur2[A, B, C, O]]] {
    val g = (a: A, b: B) => new Cur2(a, b, f)
    override def apply(a: A) = new Cur1(a, g)
    override def reusable = { case x: Fn3[A, B, C, O] => f eq x.f }
  }

  final class Cur1[A: Reusable, B, O](val a: A, val f: (A, B) => O) extends ReusableFn[B, O] {
    override def apply(b: B): O = f(a, b)
    override def reusable = { case x: Cur1[A, B, O] => (f eq x.f) && (a ~=~ x.a) }
  }

  final class Cur2[A: Reusable, B: Reusable, C, O](val a: A, val b: B, val f: (A, B, C) => O) extends ReusableFn[C, O] {
    override def apply(c: C): O = f(a, b, c)
    override def reusable = { case x: Cur2[A, B, C, O] => (f eq x.f) && (a ~=~ x.a) && (b ~=~ x.b) }
  }
}
