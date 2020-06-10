package shipreq.base.util

import utest._
import nyaya.prop._
import nyaya.test._
import PropTest._

object IntersectionTest extends TestSuite {

  class Laws[A, B](a: A, b: B, i: Intersection[A, B]) {
    val E = EvalOver((a, b))

    def reverse =
      new Laws[B, A](b, a, i.reverse)

    def eval =
      E.atom("get = reverseGet", {
        i.getOption(a).map(i.reverse.getOption(_)) match {
          case None | Some(Some(`a`)) => None
          case x => Some(s"Got: $x")
        }
      }) &
      E.test("i.reverse.reverse eq i", i.reverse.reverse eq i)
  }

  val ic = Intersection[Int, Char] {
    case 0  => Some('0')
    case 10 => None
  } {
    case '0' => Some(0)
    case 'a' => None
  }


  val domain: Domain[Laws[Int, Char]] = {
    val i = Domain.ofValues(0, 10)
    val c = Domain.ofValues('0', 'a')
    (i *** c).map(x => new Laws(x._1, x._2, ic))
  }

  override def tests = Tests {
    "forwards" - { domain mustProve Prop.eval(_.eval) }
    "backwards" - { domain.map(_.reverse) mustProve Prop.eval(_.eval) }

    "toOption" - {
      val d: Domain[Laws[Int, Option[Char]]] = {
        val i = Domain.ofValues(0, 10)
        val c = Domain.ofValues(Some('0'), Some('a'), None)
        val ico = ic <=> Intersection.toOption[Char]
        (i *** c).map(x => new Laws(x._1, x._2, ico))
      }
      d mustProve Prop.eval(_.eval)
    }

  }
}
