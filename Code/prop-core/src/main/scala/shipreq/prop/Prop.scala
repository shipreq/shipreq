package shipreq.prop

import scala.annotation.elidable
import scalaz.{Foldable, NonEmptyList, Need, Equal}
import scalaz.std.tuple.tuple2Monoid
import scalaz.std.list.listMonoid
import scalaz.std.set.setMonoid
import scalaz.syntax.equal._
import scalaz.syntax.foldable._
import shipreq.prop.util.{Monoidmap, Multimap}

object Prop {

  def apply[A](name: String, t: A => Boolean): Prop[A] =
    atom(name, a => if (t(a)) None else Some(fmta(a)))

  def atom[A](name: String, t: A => Option[FailureInfo]): Prop[A] =
    new Atom[A](name, t)

  def equalSelf[A: Equal](name: String, f: A => A): Prop[A] =
    equal[A, A](name, f, identity)

  def equal[A, B: Equal](name: String, t: A => B, e: A => B): Prop[A] =
    atom[A](name, a => {
      val ba = t(a)
      val be = e(a)
      test(ba ≟ be, s"Actual: $ba\nExpect: $be")
    })

  def equal[A](name: String) = new EqualB[A](name)
  final class EqualB[A](name: String) {
    def apply[B: Equal](t: A => B, e: A => B): Prop[A] =
      equal(name, t, e)
  }

  def ifelse[A](test: Prop[A], ifPass: Prop[A], ifFail: Prop[A]): Prop[A] =
    (test ==> ifPass) ∧ (~test ==> ifFail)

  def distinct[A, B](name: String, f: A => Stream[B]) =
    distinct[B](name).contramap(f)

  def distinct[A](name: String) =
    atom[Stream[A]](s"each $name is unique", as => {
      val dups = (Map.empty[A, Int] /: as)((q, a) => q + (a -> (q.getOrElse(a, 0) + 1))).filter(_._2 > 1)
      if (dups.isEmpty)
        None
      else
        Some(Need {
          val d = dups.toStream
            .sortBy(_._1.toString)
            .map { case (a, i) => s"$a → $i"}
            .mkString("{", ", ", "}")
          s"Inputs: $as\nDups: $d"
        })
    })

  /** Ensures that A's Cs form a subset of A's Bs. */
  def subset[A](name: String) = new SubsetB[A](name)
  final class SubsetB[A](val name: String) extends AnyVal {
    def apply[B, C](superset: A => Set[B], sub: A => Traversable[C])(implicit ev: C <:< B) =
      atom[A](name, a => {
        val valid = superset(a)
        val found = sub(a)
        val bad = (Set.empty[C] /: found)((q, c) => if (valid contains c) q else q + c)
        if (bad.isEmpty)
          None
        else
          Some(Need {
            val b = bad.toStream.map(_.toString).sorted.distinct.mkString("{", ", ", "}")
            s"$a\nLegal: (${valid.size}) $valid\nFound: (${found.size}) $found\nIllegal: $b"
          })
      })
  }

  def test(b: Boolean, e: => String): Option[FailureInfo] =
    if (b) None else Some(Need(e))

  private[prop] def fmta(a: Any): FailureInfo =
    Need(a.toString)

  private[Prop] def failureExpected(a: Any): FailureInfo =
    Need(s"Failure expected with input [$a]")

  private[prop] def testB[A](a: A, test: Boolean): Option[FailureInfo] =
    if (test) None else Some(fmta(a))
}

import Prop.{failureExpected, testB, fmta}


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

  def testE(a: A): Option[FailureInfo]
  final def test(a: A): Boolean = testE(a).isEmpty

  protected[prop] def wantFailure(a: A): Option[FailureInfo] =
    if (test(a)) Some(failureExpected(a)) else None

  final def falsify(a: A) = falsifyE(a, true)

  @elidable(elidable.ASSERTION)
  final def assert(a: A): Unit =
    falsify(a).foreach(f => {
      val err = f.report
      val sep = "=" * 120
      System.err.println(sep)
      System.err.println(err)
      System.err.println(sep)
      throw new java.lang.AssertionError(err)
    })

  def falsifyE: (A, Boolean) => Option[Falsification[A]]

  @inline protected final def falsifyX(f: (A, Boolean) => List[Falsification[A]]): (A, Boolean) => Option[Falsification[A]] =
    (a,e) =>
      testE(a) match {
      case Some(i) if e  => Some(Falsification(this, f(a,e), Set(i)))
      case None    if !e => Some(Falsification(this, f(a,e), Set(failureExpected(a))))
      case _             => None
    }

  @inline protected final def falsifyN =
    falsifyX((a,e) => Nil)

  @inline protected final def falsifyB(ps: NonEmptyList[Prop[A]]) =
    falsifyX((a, e) => ps.list.flatMap(_.falsifyE(a, e)).toList)

  @inline protected final def falsifyP(p: Prop[A], e: Boolean) =
    falsifyX((a, e2) => p.falsifyE(a, e == e2).toList)
}


final case class Atom[A](name: String, t: A => Option[FailureInfo]) extends Prop[A] {
  override def testE(a: A) = t(a)
  override def falsifyE = falsifyN
  override def toString = name
}


final case class Negation[A](p: Prop[A]) extends Prop[A] {
  override def unary_~ = p
  override def testE(a: A) = p.wantFailure(a)
  override def falsifyE = falsifyN //falsifyP(p, false)
  override def toString = s"¬$p"
}


final case class Disjunction[A](ps: NonEmptyList[Prop[A]]) extends Prop[A] {
  override def |(q: Prop[A]) = Disjunction(q <:: ps)
  override def testE(a: A) = testB(a, ps.stream.exists(_.test(a)))
  override def falsifyE = falsifyN //falsifyB(ps)
  override def toString = ps.stream.map(_.toString).mkString(" ∨ ")
}

final case class Conjunction[A](ps: NonEmptyList[Prop[A]]) extends Prop[A] {
  override def &(q: Prop[A]) = Conjunction(q <:: ps)
  override def testE(a: A) = testB(a, ps.stream.forall(_.test(a)))
  override def falsifyE = falsifyB(ps)
  override def toString = ps.stream.map(_.toString).mkString(" ∧ ")
}


final case class Implication[A](a: Prop[A], c: Prop[A]) extends Prop[A] {
  override def testE(i: A) = testB(i, !a.test(i) || c.test(i))
  override def falsifyE = falsifyP(c, true)
  override def toString = s"$a ⇒ $c"
}


final case class Reduction[A](c: Prop[A], a: Prop[A]) extends Prop[A]  {
  override def testE(i: A) = testB(i, !a.test(i) || c.test(i))
  override def falsifyE = falsifyP(c, true)
  override def toString = s"$c ⇐ $a"
}


final case class Biconditional[A](p: Prop[A], q: Prop[A]) extends Prop[A]  {
  override def testE(a: A) = testB(a, p.test(a) == q.test(a))
  override def falsifyE = falsifyN
  override def toString = s"$p ⇔ $q"
}


final case class Rename[A](name: String, p: Prop[A]) extends Prop[A] {
  override def rename(n: String): Prop[A] = Rename(n, p)
  override def testE(a: A) = p.testE(a)
  override def falsifyE = (a,e) => p.falsifyE(a,e).map(_.copy(this))
  override def toString = name
}


final case class Contramap[A, B](p: Prop[B], f: A => B) extends Prop[A] {
  override def contramap[Z](g: Z => A): Prop[Z] = Contramap(p, f compose g)
  override def testE(a: A) = p.testE(f(a))
  override def falsifyE = (a,e) => p.falsifyE(f(a), e).map(_.map(_ contramap f))
  override def toString = p.toString
}


final case class Forall[F[_]: Foldable, A, B](p: Prop[B], f: A => F[B], updName: Boolean) extends Prop[A] {
  override def testE(a: A) = testB(a, f(a).∀(b => p.test(b)))
  override def falsifyE = (a, e) =>
    f(a).foldl(List.empty[Falsification[B]])(q => b => p.falsifyE(b, e).toList ::: q) match {
      case Nil => None
      case fs@(_ :: _) =>
        val causes = fs
          .foldLeft(Monoidmap.empty[Prop[B], (List[Falsification[B]], Set[FailureInfo])])(
            (q, i) => q.add(i.p, (i.cause, i.finfo)))
          .plainMap.toList
          .map { case (p, (cs, is)) =>
            val causes2 = cs
              .foldLeft(Multimap.empty[Prop[B], Set, FailureInfo])((q, c) => q.addvs(c.p, c.finfo))
              .m.toList
              .map { case (p2, is2) => Falsification(p2, Nil, is2) }
            val inputs2 = causes2.foldLeft(Set.empty[FailureInfo])(_ ++ _.finfo)
            Falsification(p, causes2, inputs2).map[A](Forall(_, f, false))
          }
        Some(Falsification(this, causes, Set(fmta(a))))
    }
  override def toString = if (updName) s"∀{$p}" else p.toString
}