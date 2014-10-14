package shipreq.base.prop

import scala.annotation.tailrec
import scalaz.NonEmptyList
import shipreq.base.util.Util

sealed abstract class Prop[A] {
  def unary_~                 : Prop[A] = Negation(this)
  def |           (q: Prop[A]): Prop[A] = Disjunction(NonEmptyList(q, this))
  def &           (q: Prop[A]): Prop[A] = Conjunction(NonEmptyList(q, this))
  def ==>         (c: Prop[A]): Prop[A] = Implication(this, c)
  def <==         (a: Prop[A]): Prop[A] = Reduction(this, a)
  def <==>        (q: Prop[A]): Prop[A] = Biconditional(this, q)
  def contramap[Z](f: Z => A) : Prop[Z] = Contramap(this, f)

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

  def falsifyE: (Ctx[A], Boolean) => Option[Falsification[A]]

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
  override def falsifyE = falsifyN
  override def toString = name
}


final case class Contramap[A, B](p: Prop[B], f: A => B) extends Prop[A] {
  override def contramap[Z](g: Z => A): Prop[Z] = Contramap(p, f compose g)
  override def test(x: Ctx[A]) = p.test(x map f)
  override def falsifyE = (x,e) => p.falsifyE(x map f, e).map(_ contramap f)
  override def toString = p.toString
}


final case class Negation[A](p: Prop[A]) extends Prop[A] {
  override def unary_~ = p
  override def test(x: Ctx[A]) = !p.test(x)
  override def falsifyE = falsifyN //falsifyP(p, false)
  override def toString = s"¬$p"
}


final case class Disjunction[A](ps: NonEmptyList[Prop[A]]) extends Prop[A] {
  override def |(q: Prop[A]) = Disjunction(q <:: ps)
  override def test(x: Ctx[A]) = ps.stream.exists(_ test x)
  override def falsifyE = falsifyB(ps)
  override def toString = ps.stream.map(_.toString).mkString(" ∨ ")
}

final case class Conjunction[A](ps: NonEmptyList[Prop[A]]) extends Prop[A] {
  override def &(q: Prop[A]) = Conjunction(q <:: ps)
  override def test(x: Ctx[A]) = ps.stream.forall(_ test x)
  override def falsifyE = falsifyB(ps)
  override def toString = ps.stream.map(_.toString).mkString(" ∧ ")
}


final case class Implication[A](a: Prop[A], c: Prop[A]) extends Prop[A] {
  override def test(x: Ctx[A]) = !a.test(x) || c.test(x)
  override def falsifyE = falsifyP(c, true)
  override def toString = s"$a ⇒ $c"
}


final case class Reduction[A](c: Prop[A], a: Prop[A]) extends Prop[A]  {
  override def test(x: Ctx[A]) = !a.test(x) || c.test(x)
  override def falsifyE = falsifyP(c, true)
  override def toString = s"$c ⇐ $a"
}


final case class Biconditional[A](p: Prop[A], q: Prop[A]) extends Prop[A]  {
  override def test(x: Ctx[A]) = p.test(x) == q.test(x)
  override def falsifyE = falsifyN
  override def toString = s"$p ⇔ $q"
}


object Prop {

  @inline final def apply[A](name: String, t: A => Boolean): Prop[A] =
    withCtx(name, x => t(x.a))

  @inline final def withCtx[A](name: String, t: Ctx[A] => Boolean): Prop[A] =
    new Atom[A](name, t)
}

final case class Falsification[A](p: Prop[A], cause: List[Falsification[A]]) {

  def contramap[Z](f: Z => A): Falsification[Z] =
    Falsification(p contramap f, cause map (_ contramap f))

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
    val pm = "│  "
    val pl = "   "
    val cm = "├─ "
    val cl = "└─ "
    def loop(parentLvlLast: Vector[Boolean], fs: List[Falsification[A]], root: Boolean): Unit = fs match {
      case Nil =>
      case h :: t =>
        sb append '\n'
        for (b <- parentLvlLast) sb.append(if (b) pl else pm)
        val last = t.isEmpty
        if (!root) sb.append(if (last) cl else cm)
        sb append h.p.toString
        val nextLvl = if (root) Vector.empty[Boolean] else parentLvlLast :+ last
        loop(nextLvl, h.cause, false)
        loop(parentLvlLast, t, root)
    }
    loop(Vector.empty, List(this), true)
  }
}
