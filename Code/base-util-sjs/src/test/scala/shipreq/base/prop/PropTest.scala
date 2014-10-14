package shipreq.base.prop

import scalaz.std.list._
import utest._

object PropTest extends TestSuite {
  val even = Prop[Int]("even", _ % 2 == 0)
  val mod3 = Prop[Int]("mod3", _ % 3 == 0)
  val mod5 = Prop[Int]("mod5", _ % 5 == 0)
  val odd = ~even
  val mod235d = even | mod3 | mod5
  val mod235c = even & mod3 & mod5
  val evenF = Falsification(even, Nil)
  val oddF = Falsification(odd, Nil)
  val mod3F = Falsification(mod3, Nil)
  val mod5F = Falsification(mod5, Nil)

  override def tests = TestSuite {
    'Falsification {
      def test[A](p: Prop[A], a: A, e: Option[Falsification[A]]): Unit = {
        def s(f: Falsification[A]): Falsification[A] =
          f.copy(cause = f.cause.sortBy(_.toString).map(s))
        val actual = p falsify1 a map s
        val expect = e map s
        assert(actual == expect)
      }
      def testRootCauses[A](p: Prop[A], a: A, e: List[Prop[_]]): Unit = {
        val r = p.falsify1(a).toList.flatMap(_.rootCauses.list)
        val actual = r.map(_.toString).sortBy(_.toString)
        val expect = e.map(_.toString).sortBy(_.toString)
        assert(actual == expect)
      }
      'atom {
        test(even, 2, None)
        test(even, 3, Some(evenF))
      }
      'negation {
        test(odd, 3, None)
        // test(odd, 2, Some(Falsification(odd, List(evenF))))
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
        test(mod235d, 31, Some(Falsification(mod235d, List(evenF, mod3F, mod5F))))
      }
      'conjunction {
        test(mod235c, 30, None)
        test(mod235c, 31, Some(Falsification(mod235c, List(evenF, mod3F, mod5F))))
        test(mod235c, 15, Some(Falsification(mod235c, List(evenF))))
        test(mod235c,  4, Some(Falsification(mod235c, List(mod3F, mod5F))))
      }
      'implication {
        val mod5impEven = mod5 ==> even
        test(mod5impEven, 1, None)
        test(mod5impEven, 10, None)
        test(mod5impEven, 5, Some(Falsification(mod5impEven, List(evenF))))
      }
      'reduction {
        val evenRedMod5 = even <== mod5
        test(evenRedMod5,  1, None)
        test(evenRedMod5, 10, None)
        test(evenRedMod5,  5, Some(Falsification(evenRedMod5, List(evenF))))
      }
      'biconditional {
        val evenIffMod5 = even <==> mod5
        test(evenIffMod5, 10, None)
        test(evenIffMod5,  5, Some(Falsification(evenIffMod5, Nil)))
        test(evenIffMod5,  2, Some(Falsification(evenIffMod5, Nil)))
      }
      'nested {
        val a = even ∧ odd
        val b = even ==> a
        val c = mod5 ∧ b
        val p = mod5 ∧ c
        test(p, 10, Some(Falsification(p, List(
          Falsification(c, List(
            Falsification(b, List(
              Falsification(a, List(
                Falsification(odd, List(
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
    }
  }
}