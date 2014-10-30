package shipreq.base.prop

import scalaz.std.list._
import utest._

object PropTest extends TestSuite {

  final class Falsy[A] private[Falsy] (val p: Prop[A], val cause: List[Falsy[A]]) {
    override def equals(o: Any) = o match {
      case that: Falsy[_] => p == that.p && cause == that.cause
      case _ => false
    }
    override def hashCode = p.hashCode
    override def toString = s"Falsy($p, $cause)"
  }
  object Falsy {
    def apply[A](p: Prop[A], cause: List[Falsy[A]]): Falsy[A] =
      new Falsy(p, cause.sortBy(_.toString))
  }
  implicit def toFalsy[A](f: Falsification[A]): Falsy[A] = Falsy(f.p, f.cause map toFalsy)

  def norm[A](f: Falsification[A]): Falsification[A] =
    Falsification(f.p, f.cause.sortBy(_.toString) map norm, f.inputs)

  val even = Prop[Int]("even", _ % 2 == 0)
  val mod3 = Prop[Int]("mod3", _ % 3 == 0)
  val mod5 = Prop[Int]("mod5", _ % 5 == 0)
  val odd = ~even
  val mod235d = even | mod3 | mod5
  val mod235c = even & mod3 & mod5
  val evenF = Falsy(even, Nil)
  val oddF  = Falsy(odd, Nil)
  val mod3F = Falsy(mod3, Nil)
  val mod5F = Falsy(mod5, Nil)

  override def tests = TestSuite {
    'Falsy {
      def test[A](p: Prop[A], a: A, expect: Option[Falsy[A]]): Unit = {
        val actual = p falsify1 a map toFalsy
        assert(actual == expect)
      }
      def testRootCauses[A](p: Prop[A], a: A, e: List[Prop[_]]): Unit = {
        val r = p.falsify1(a).toList.flatMap(_.rootCauses.list)
        val actual = r.map(_.toString).sorted
        val expect = e.map(_.toString).sorted
        assert(actual == expect)
      }
      'atom {
        test(even, 2, None)
        test(even, 3, Some(evenF))
      }
      'negation {
        test(odd, 3, None)
        // test(odd, 2, Some(Falsy(odd, List(evenF))))
        test(odd, 2, Some(oddF))
      }
      'doubleNegation {
        val p  = ~(~even)
        test(p, 2, None)
        test(p, 3, Some(evenF))
      }
      'disjunction {
        test(mod235d, 30, None)
        test(mod235d,  4, None)
        // test(mod235d, 31, Some(Falsy(mod235d, List(evenF, mod3F, mod5F))))
        test(mod235d, 31, Some(Falsy(mod235d, Nil)))
      }
      'conjunction {
        test(mod235c, 30, None)
        test(mod235c, 31, Some(Falsy(mod235c, List(evenF, mod3F, mod5F))))
        test(mod235c, 15, Some(Falsy(mod235c, List(evenF))))
        test(mod235c,  4, Some(Falsy(mod235c, List(mod3F, mod5F))))
      }
      'implication {
        val mod5impEven = mod5 ==> even
        test(mod5impEven, 1, None)
        test(mod5impEven, 10, None)
        test(mod5impEven, 5, Some(Falsy(mod5impEven, List(evenF))))
      }
      'reduction {
        val evenRedMod5 = even <== mod5
        test(evenRedMod5,  1, None)
        test(evenRedMod5, 10, None)
        test(evenRedMod5,  5, Some(Falsy(evenRedMod5, List(evenF))))
      }
      'biconditional {
        val evenIffMod5 = even <==> mod5
        test(evenIffMod5, 10, None)
        test(evenIffMod5,  5, Some(Falsy(evenIffMod5, Nil)))
        test(evenIffMod5,  2, Some(Falsy(evenIffMod5, Nil)))
      }
      'nested {
        val a = even ∧ odd
        val b = even ==> a
        val c = mod5 ∧ b
        val p = mod5 ∧ c
        test(p, 10, Some(Falsy(p, List(
          Falsy(c, List(
            Falsy(b, List(
              Falsy(a, List(
                Falsy(odd, List(
                  ))))))))))))
      }
      'rootCause {
        val p = mod5 ∧ (even ==> (even ∧ odd)) ∧ (mod3 ∧ ~even)
        testRootCauses(p, 10, List(odd, mod3))
        testRootCauses(mod235c, 10, List(mod3))
      }
      'contramap {
        val upper = Prop[String]("upper", s => s == s.toUpperCase)
        case class Yay(s: String, i: Int)
        val p = mod235c.contramap[Yay](_.i) ∧ upper.contramap[Yay](_.s)
        testRootCauses(p, Yay("GOOD", 30), Nil)
        testRootCauses(p, Yay("Bad", 30), List(upper))
        testRootCauses(p, Yay("GOOD", 15), List(even))
        testRootCauses(p, Yay("both", 4), List(mod3, mod5, upper))
      }
      'forall {
        val allEven = even.forallF[List]
        testRootCauses(allEven, List(4,6), Nil)
        testRootCauses(allEven, Nil, Nil)
        testRootCauses(allEven, List(4,5,6,7), List(even))
      }
      'renamed {
        val t = mod235c.rename("whateverness").falsify1(6).get.failureTree
        assert(t == "whateverness\n└─ mod5")
      }
      'forall2 {
        val t = Prop[Int]("true", _ => true)
        val q = even ∧ mod5 ∧ t
        val p = q.forallF[List].asInstanceOf[Forall[List, List[Int], Int]]
        val r = p.falsify1(List(1,2,3,4,5,6,7,8,9,10)).get
        val d = Falsification(mod5, Nil, Set(1,2,3,4,6,7,8,9))
        val e = Falsification(even, Nil, Set(1,3,5,7,9))
        val c = Falsification(q, List(d, e), Set(1,2,3,4,5,6,7,8,9))
        val f = Falsification[List[Int]](p, List(c.map(x => Forall(x, p.f, false))), Set(List(1,2,3,4,5,6,7,8,9,10)))
        val List(r2,f2) = List(r,f) map norm
        assert(r2 == f2)
      }
    }
  }
}