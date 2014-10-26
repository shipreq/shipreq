package shipreq.base.prop

import scala.annotation.{elidable, tailrec}
import scalaz.{Foldable, NonEmptyList}
import scalaz.syntax.foldable._

object Prop {

  @inline final def apply[A](name: String, t: A => Boolean): Prop[A] =
    atom2(name, t, identity)

  @inline final def atom2[A](name: String, t: A => Boolean, fmta: A => Any): Prop[A] =
    atom(name, (a,_) => t(a), fmta)

  @inline final def atom[A](name: String, t: (A, Ctx) => Boolean, fmta: A => Any): Prop[A] =
    new Atom[A](name, t, fmta)

  def distinct[A, B](name: String, f: A => Stream[B]) =
    distinct[B](name).contramap(f)

  def distinct[A](name: String) =
    atom2[Stream[A]](s"each $name is unique", as => {
      var s = Set.empty[A]
      !as.exists(a =>
        s.contains(a) match {
          case true  => true
          case false => s += a; false
        })
    }, as => {
      val dups =
        (Map.empty[A, Int] /: as)((q, a) => q + (a -> (q.getOrElse(a, 0) + 1)))
          .filter(_._2 > 1)
          .toStream
          .sortBy(_._1.toString)
          .map { case (a, i) => s"$a -> $i"}
          .mkString("Dups(", ", ", ")")
      s"$as\n$dups"
    })
}


sealed abstract class Prop[A] {
  def unary_~                                : Prop[A] = Negation(this)
  def |                        (q: Prop[A])  : Prop[A] = Disjunction(NonEmptyList(q, this))
  def &                        (q: Prop[A])  : Prop[A] = Conjunction(NonEmptyList(q, this))
  def ==>                      (c: Prop[A])  : Prop[A] = Implication(this, c)
  def <==                      (a: Prop[A])  : Prop[A] = Reduction(this, a)
  def <==>                     (q: Prop[A])  : Prop[A] = Biconditional(this, q)

  def rename(n: String): Prop[A] = Rename(n, this)

  def contramap[Z](f: Z => A): Prop[Z] = Contramap(this, f)

  def forall[Z, F[_] : Foldable](f: Z => F[A]): Prop[Z] = Forall(this, f, true)

  def forallF[F[_] : Foldable] = forall[F[A], F](f => f)

  @inline final def subst[B <: A]: Prop[B] = contramap(a => a: B)

  @inline final def ∨                   (q: Prop[A])   = this | q
  @inline final def ∧                   (q: Prop[A])   = this & q
  @inline final def ⇐                   (a: Prop[A])   = this <== a
  @inline final def ⇔                   (q: Prop[A])   = this <==> q
  @inline final def iff                 (q: Prop[A])   = this <==> q
  @inline final def or                  (q: Prop[A])   = this | q
  @inline final def and                 (q: Prop[A])   = this & q
  @inline final def implies             (c: Prop[A])   = this ==> c
  @inline final def ∀[Z, F[_]: Foldable](f: Z => F[A]) = forall(f)

  def test(a: A, x: Ctx): Boolean
  final def test1(a: A) = test(a, Ctx.single)

  final def falsify(a: A, x: Ctx) = falsifyE(a, x, true)
  final def falsify1(a: A) = falsify(a, Ctx.single)

  @elidable(elidable.ASSERTION)
  final def assert1(a: A): Unit =
    falsify1(a).foreach(f => {
      val err = f.report
      val sep = "=" * 120
      System.err.println(sep)
      System.err.println(err)
      System.err.println(sep)
      throw new java.lang.AssertionError(err)
    })

  protected[prop] def fmta: A => Any

  def falsifyE: (A, Ctx, Boolean) => Option[Falsification[A]]

  @inline protected final def falsifyX(f: (A, Ctx, Boolean) => List[Falsification[A]]): (A, Ctx, Boolean) => Option[Falsification[A]] =
    (a,x,e) => if (test(a,x) == e) None else Some(Falsification(this, f(a,x,e), Set(fmta(a))))

  @inline protected final def falsifyN =
    falsifyX((a,x,e) => Nil)

  @inline protected final def falsifyB(ps: NonEmptyList[Prop[A]]) =
    falsifyX((a, x, e) => ps.list.flatMap(_.falsifyE(a, x, e)).toList)

  @inline protected final def falsifyP(p: Prop[A], e: Boolean) =
    falsifyX((a, x, e2) => p.falsifyE(a, x, e == e2).toList)
}


final case class Atom[A](name: String, t: (A, Ctx) => Boolean, fmta: A => Any) extends Prop[A] {
  override def test(a: A, x: Ctx) = t(a, x)
  override def falsifyE = falsifyN
  override def toString = name
}


final case class Negation[A](p: Prop[A]) extends Prop[A] {
  override def unary_~ = p
  override def test(a: A, x: Ctx) = !p.test(a, x)
  override def falsifyE = falsifyN //falsifyP(p, false)
  override def toString = s"¬$p"
  override protected[prop] def fmta = p.fmta
}


final case class Disjunction[A](ps: NonEmptyList[Prop[A]]) extends Prop[A] {
  override def |(q: Prop[A]) = Disjunction(q <:: ps)
  override def test(a: A, x: Ctx) = ps.stream.exists(_.test(a, x))
  override def falsifyE = falsifyN //falsifyB(ps)
  override def toString = ps.stream.map(_.toString).mkString(" ∨ ")
  override protected[prop] def fmta = identity
}

final case class Conjunction[A](ps: NonEmptyList[Prop[A]]) extends Prop[A] {
  override def &(q: Prop[A]) = Conjunction(q <:: ps)
  override def test(a: A, x: Ctx) = ps.stream.forall(_.test(a, x))
  override def falsifyE = falsifyB(ps)
  override def toString = ps.stream.map(_.toString).mkString(" ∧ ")
  override protected[prop] def fmta = identity
}


final case class Implication[A](a: Prop[A], c: Prop[A]) extends Prop[A] {
  override def test(i: A, x: Ctx) = !a.test(i, x) || c.test(i, x)
  override def falsifyE = falsifyP(c, true)
  override def toString = s"$a ⇒ $c"
  override protected[prop] def fmta = identity
}


final case class Reduction[A](c: Prop[A], a: Prop[A]) extends Prop[A]  {
  override def test(i: A, x: Ctx) = !a.test(i, x) || c.test(i, x)
  override def falsifyE = falsifyP(c, true)
  override def toString = s"$c ⇐ $a"
  override protected[prop] def fmta = identity
}


final case class Biconditional[A](p: Prop[A], q: Prop[A]) extends Prop[A]  {
  override def test(a: A, x: Ctx) = p.test(a, x) == q.test(a, x)
  override def falsifyE = falsifyN
  override def toString = s"$p ⇔ $q"
  override protected[prop] def fmta = identity
}


final case class Rename[A](name: String, p: Prop[A]) extends Prop[A] {
  override def rename(n: String): Prop[A] = Rename(n, p)
  override def test(a: A, x: Ctx) = p.test(a, x)
  override def falsifyE = (a,b,c) => p.falsifyE(a,b,c).map(_.copy(this))
  override def toString = name
  override protected[prop] def fmta = p.fmta
}


final case class Contramap[A, B](p: Prop[B], f: A => B) extends Prop[A] {
  override def contramap[Z](g: Z => A): Prop[Z] = Contramap(p, f compose g)
  override def test(a: A, x: Ctx) = p.test(f(a), x)
  override def falsifyE = (a,x,e) => p.falsifyE(f(a), x, e).map(_.map(_ contramap f))
  override def toString = p.toString
  override protected[prop] def fmta = p.fmta compose f
}


final case class Forall[F[_]: Foldable, A, B](p: Prop[B], f: A => F[B], updName: Boolean) extends Prop[A] {
  override def test(a: A, x: Ctx) = f(a).∀(b => p.test(b, x))
  override def falsifyE = (a, x, e) =>
    f(a).foldl(List.empty[Falsification[B]])(q => b => p.falsifyE(b, x, e).toList ::: q) match {
      case Nil => None
      case fs@(_ :: _) =>
        val causes = fs
          .foldLeft(Map.empty[Prop[B], (List[Falsification[B]], Set[Any])])((q, i) => {
          val (cs, is) = q.getOrElse(i.p, (List.empty[Falsification[B]], Set.empty[Any]))
          q + (i.p ->(cs ++ i.cause, is ++ i.inputs))
        })
          .toList
          .map { case (p, (cs, is)) =>
          val causes2 = cs
            .foldLeft(Map.empty[Prop[B], Set[Any]])((q, c) =>
            q + (c.p -> (q.getOrElse(c.p, Set.empty[Any]) ++ c.inputs)))
            .toList.map { case (p2, is2) => Falsification(p2, Nil, is2) }
          val inputs2 = causes2.foldLeft(Set.empty[Any])(_ ++ _.inputs)
          Falsification(p, causes2, inputs2).map[A](Forall(_, f, false))
        }
        Some(Falsification(this, causes, Set(a)))
    }
  override def toString = if (updName) s"∀{$p}" else p.toString
  override protected[prop] def fmta = identity
}