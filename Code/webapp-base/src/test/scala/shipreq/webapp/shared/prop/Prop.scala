package shipreq.webapp.shared.prop

import shipreq.base.util.Util

import scala.annotation.tailrec
import scalaz.NonEmptyList

sealed abstract class Prop[A] {
  def unary_~ : Prop[A] = Negation(this)
  def |   (q: Prop[A]) = Disjunction(NonEmptyList(q, this))
  def &   (q: Prop[A]) = Conjunction(NonEmptyList(q, this))
  def ==> (c: Prop[A]) = Implication(this, c)
  def <== (a: Prop[A]) = Reduction(this, a)
  def <==>(q: Prop[A]) = Biconditional(this, q)

  @inline final def unary_¬             = ~this
  @inline final def ∨      (q: Prop[A]) = this | q
  @inline final def ∧      (q: Prop[A]) = this & q
  @inline final def ⇐      (a: Prop[A]) = this <== a
  @inline final def ⇔      (q: Prop[A]) = this <==> q
  @inline final def iff    (q: Prop[A]) = this <==> q
  @inline final def or     (q: Prop[A]) = this | q
  @inline final def and    (q: Prop[A]) = this & q
  @inline final def implies(c: Prop[A]) = this ==> c

  def test(x: Ctx[A]): Boolean
  final def test1(a: A) = test(Ctx single a)

  final def falsify(x: Ctx[A]) = falsifyE(x, true)
  final def falsify1(a: A) = falsify(Ctx single a)

  protected def falsifyE: (Ctx[A], Boolean) => Option[Falsification[A]]

  @inline protected final def falsifyX(f: (Ctx[A], Boolean) => List[Falsification[A]]): (Ctx[A], Boolean) => Option[Falsification[A]] =
    (x,e) => if (test(x) == e) None else Some(Falsification(this, f(x,e)))

  @inline protected final def falsifyN =
    falsifyX((x,e) => Nil)

  @inline protected final def falsifyB(ps: NonEmptyList[Prop[A]]) =
    falsifyX((x, e) => ps.list.flatMap(_.falsifyE(x, e)).toList)

  @inline protected final def falsifyP(p: Prop[A], e: Boolean) =
    falsifyX((x, e2) => p.falsifyE(x, e == e2).toList)
}

final case class Atom[A](name: String, t: Ctx[A] => Boolean) extends Prop[A] {
  override def test(x: Ctx[A]) = t(x)
  override protected def falsifyE = falsifyN
  override def toString = name
}

final case class Negation[A](p: Prop[A]) extends Prop[A] {
  override def unary_~ = p

  override def test(x: Ctx[A]) = !p.test(x)
  override protected def falsifyE = falsifyN //falsifyP(p, false)
  override def toString = s"¬$p"
}

final case class Disjunction[A](ps: NonEmptyList[Prop[A]]) extends Prop[A] {
  override def |(q: Prop[A]) = Disjunction(q <:: ps)

  override def test(x: Ctx[A]) = ps.stream.exists(_ test x)
  override protected def falsifyE = falsifyB(ps)
  override def toString = ps.stream.map(_.toString).mkString(" ∨ ")
}

final case class Conjunction[A](ps: NonEmptyList[Prop[A]]) extends Prop[A] {
  override def &(q: Prop[A]) = Conjunction(q <:: ps)

  override def test(x: Ctx[A]) = ps.stream.forall(_ test x)
  override protected def falsifyE = falsifyB(ps)
  override def toString = ps.stream.map(_.toString).mkString(" ∧ ")
}

final case class Implication[A](a: Prop[A], c: Prop[A]) extends Prop[A] {
  override def test(x: Ctx[A]) = !a.test(x) || c.test(x)
  override protected def falsifyE = falsifyP(c, true)
  override def toString = s"$a ⇒ $c"
}

final case class Reduction[A](c: Prop[A], a: Prop[A]) extends Prop[A]  {
  override def test(x: Ctx[A]) = !a.test(x) || c.test(x)
  override protected def falsifyE = falsifyP(c, true)
  override def toString = s"$c ⇐ $a"
}

final case class Biconditional[A](p: Prop[A], q: Prop[A]) extends Prop[A]  {
  override def test(x: Ctx[A]) = p.test(x) == q.test(x)
  override protected def falsifyE = falsifyN
  override def toString = s"$p ⇔ $q"
}

object Prop {

  @inline final def apply[A](name: String, t: A => Boolean): Prop[A] =
    withCtx(name, x => t(x.a))

  @inline final def withCtx[A](name: String, t: Ctx[A] => Boolean): Prop[A] =
    new Atom[A](name, t)
}

case class Falsification[A](p: Prop[A], cause: List[Falsification[A]]) {

  def rootCauses: NonEmptyList[Prop[A]] = {
    @tailrec
    def loop2(fs: List[Falsification[A]], cs: List[Prop[A]]): List[Prop[A]] =
      fs match {
        case Nil => cs
        case h :: t => loop2(t, loop(h, cs).list)
      }
    @tailrec
    def loop(f: Falsification[A], cs: List[Prop[A]]): NonEmptyList[Prop[A]] =
      f match {
        case Falsification(p, Nil) => NonEmptyList.nel(p, cs.filterNot(_ == p))
        case Falsification(_, h :: t) => loop(h, loop2(t, cs))
      }
    loop(this, Nil)
  }

  def tree: String = Util.quickSB(treeSB)
  def treeSB(sb: StringBuilder): Unit = {
  }
}
