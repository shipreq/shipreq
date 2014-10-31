package shipreq.prop.test

import shipreq.prop.util.Util
import shipreq.prop._
import java.lang.AssertionError
import Console.{RED, RED_B, WHITE_B, RESET}
import DefaultSettings._

object TestUtil {

  val jvm = try { "!" matches """\p{P}""" } catch { case _: Throwable => false }

  def assertProp[A](p: Prop[A], g: Gen[A])(implicit S: Settings): Unit = {

    def fail(a: A, header: String => String, footer: String): Unit = {
      if (!S.debug) {
        val v = a.toString
        val w = Util.escapeString(v)
        println(header(v))
        if (w != v) println(s"$w\n")
      }
      println(footer)
      println()
    }

    PTest(p, g, S) match {
      case RunState(_, Satisfied()) | RunState(_, Proved()) => ()

      case RunState(runs, Falsified(a, f)) =>
        fail(a, v => s"\n${RED}Falsified $WHITE_B[$p]$RESET$RED after $runs runs with:$RESET\n$v\n", f.report)
        throw new AssertionError(s"Failed: $p")

      case RunState(runs, Error(a, e)) =>
        fail(a, v => s"\n${RED_B}Crashed $WHITE_B[$p]$RESET$RED after $runs runs with:$RESET\n$v\n", e.toString)
        throw new AssertionError(s"Failed: $p", e)
    }
  }

  implicit class PropExt[A](val p: Prop[A]) extends AnyVal {
    def mustBeSatisfiedBy(g: Gen[A]) = assertProp(p, g)
    def mustBeSatisfiedBy_[B <: A](g: Gen[B]) = assertProp(p, g.subst[A])
  }

  implicit class GenExt[A](val g: Gen[A]) extends AnyVal {
    def mustSatisfy(p: Prop[A]) = assertProp(p, g)
    def _mustSatisfy[B >: A](p: Prop[B]) = assertProp(p, g.subst[B])
  }
}