package shipreq.webapp.client.util

import japgolly.scalajs.react.ReactComponentB
import scala.runtime.AbstractFunction1
import scalaz.effect.IO
import shipreq.base.util.UnivEq

final class Reusable[A](val reusable: (A, A) => Boolean) extends AnyVal

object Reusable {
  def apply[A](f: (A, A) => Boolean): Reusable[A] =
    new Reusable(f)

  def const[A](r: Boolean): Reusable[A] =
    new Reusable((_, _) => r)

  def byUnivEq[A: UnivEq]: Reusable[A] =
    new Reusable((a, b) => a == b)

  def byRef[A <: AnyRef]: Reusable[A] =
    new Reusable((a, b) => a eq b)

  implicit val int: Reusable[Int] = byUnivEq

  implicit def option[A: Reusable]: Reusable[Option[A]] =
    apply((x, y) => x.fold(y.isEmpty)(a => y.fold(false)(a ~=~ _)))

  def caseclass4[A: Reusable, B: Reusable, C: Reusable, D: Reusable, Z](f: Z => Option[(A, B, C, D)]): Reusable[Z] =
    apply { (x, y) =>
      val (xa,xb,xc,xd) = f(x).get
      val (ya,yb,yc,yd) = f(y).get
      (xa ~=~ ya) && (xb ~=~ yb) && (xc ~=~ yc) && (xd ~=~ yd)
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
}

object ReusableFn {
  def apply[A, O](f: A => O): Fn1[A, O] = new Fn1(f)

  def apply[A: Reusable, B, O](f: (A, B) => O): Fn2[A, B, O] = new Fn2(f)

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