package shipreq.prop

import scala.annotation.elidable
import scala.collection.GenTraversable
import scalaz.{Need, Equal, Foldable, Contravariant}
import scalaz.syntax.equal._
import scalaz.syntax.foldable._

final class PropA[A] private[prop](val t: A => EvalL)

object PropA {
  implicit val propInstances: Contravariant[PropA] =
    new Contravariant[PropA] {
      override def contramap[A, B](r: PropA[A])(f: B => A): PropA[B] = new PropA[B](b => r.t(f(b)))
    }
}

object Prop {

  def pass[A](name: String = "Pass"): Prop[A] =
    test(name, _ => true)

  def atom[A](name: => String, t: A => FailureReasonO): Prop[A] =
    eval(a => Eval.atom(name, a, t(a)))

  def eval[A](q: A => EvalL): Prop[A] =
    Atom[PropA, A](new PropA(q))

  def run[A](l: Prop[A])(a: A): Eval =
    l.run(p => Eval.run(p.t(a)))

  def test[A](name: => String, t: A => Boolean): Prop[A] =
    atom(name, a => reasonBool(t(a), a))

  def equalSelf[A: Equal](name: => String, f: A => A): Prop[A] =
    equal[A, A](name, f, identity)

  def equal[A, B: Equal](name: => String, actual: A => B, expect: A => B): Prop[A] =
    atom[A](name, a => reasonEq(actual(a), expect(a)))

  def equal[A](name: => String) = new EqualB[A](name)
  final class EqualB[A](val name: String) extends AnyVal {
    def apply[B: Equal](actual: A => B, expect: A => B): Prop[A] = equal(name, actual, expect)
  }

  def reason(b: Boolean, r: => String): FailureReasonO =
    if (b) None else Some(r)

  def reasonBool(b: Boolean, input: => Any): FailureReasonO =
    reason(b, s"Invalid input [$input]")

  def reasonEq[A: Equal](a: A, e: A): FailureReasonO =
    reason(a ≟ e, s"Actual: $a\nExpect: $e")

  @elidable(elidable.ASSERTION)
  def assert[A](l: => Prop[A])(a: => A): Unit =
    l(a).assertSuccess()

  def forall[A, F[_] : Foldable, B](f: A => F[B], lb: Prop[B]): Prop[A] =
    eval[A](a => {
      val es = f(a).foldLeft(List.empty[Eval])((q, b) => run(lb)(b) :: q)
      val ho = es.headOption
      val n  = Need(ho.fold("∅")(e => s"∀{${e.name.value}}"))
      val i  = Input(a)
      val r  = es.filter(_.failure) match {
                 case Nil =>
                   Eval.success(n, i)
                 case fs@(_ :: _) =>
                   val causes = fs.foldLeft(Eval.root)((q, e) => q.add(e.name.value, List(e)))
                   Eval(n, i, causes)
               }
      r.liftL
    })

  def distinctC[C[_], A](name: => String)(implicit ev: C[A] <:< GenTraversable[A]): Prop[C[A]] =
    distinct(name, _.toStream)

  def distinct[A, B](name: => String, f: A => GenTraversable[B]): Prop[A] =
    distinct[B](name).contramap(f(_).toStream)

  def distinct[A](name: => String): Prop[Stream[A]] =
    atom[Stream[A]](s"each $name is unique", as => {
      val dups = (Map.empty[A, Int] /: as)((q, a) => q + (a -> (q.getOrElse(a, 0) + 1))).filter(_._2 > 1)
      if (dups.isEmpty)
        None
      else
        Some{
          val d = dups.toStream
            .sortBy(_._1.toString)
            .map { case (a, i) => s"$a → $i"}
            .mkString("{", ", ", "}")
          s"Inputs: $as\nDups: $d"
        }
    })

  /**
   * Test that A's Cs form a subset of (A's) Bs.
   * Detect illegal values.
   */
  @inline def subset                 [A](name: String) = new SubsetB[A](name)
  @inline def prohibitIllegalElements[A](name: String) = new SubsetB[A](name)
  final class SubsetB[A](val name: String) extends AnyVal {
    def apply[B, C](legalSuperset: A => Set[B], testData: A => Traversable[C])(implicit ev: C <:< B): Prop[A] =
      atom[A](name, a => {
        val legal = legalSuperset(a)
        val found = testData(a)
        val bad   = found.foldLeft(Set.empty[C])((q, c) => if (legal contains c) q else q + c)
        setMembershipResult(a, "Legal", legal, found, "Illegal", bad)
      })
  }

  /**
   * Test that A's Cs form a superset of (A's) Bs.
   * Detect missing values.
   */
  @inline def superset               [A](name: String) = new SupersetB[A](name)
  @inline def prohibitMissingElements[A](name: String) = new SupersetB[A](name)
  final class SupersetB[A](val name: String) extends AnyVal {
    def apply[B, C](requiredSubset: A => Traversable[B], testData: A => Set[C])(implicit ev: B <:< C): Prop[A] =
      atom[A](name, a => {
        val required = requiredSubset(a)
        val found    = testData(a)
        val missing  = required.foldLeft(Set.empty[B])((q, b) => if (found contains b) q else q + b)
        setMembershipResult(a, "Required", required, found, "Missing", missing)
      })
  }

  private[this] def fmtSet(s: Set[_]): String =
    s.toStream.map(_.toString).sorted.distinct.mkString("{", ", ", "}")

  private[this] def setMembershipResult(input: Any, expectName: String, expect: Traversable[_], found: Traversable[_],
                                        failureName: String, problems: Set[_]): FailureReasonO =
    if (problems.isEmpty)
      None
    else
      Some(s"$input\n$expectName: (${expect.size}) $expect\nFound: (${found.size}) $found\n$failureName: ${fmtSet(problems)}")
}