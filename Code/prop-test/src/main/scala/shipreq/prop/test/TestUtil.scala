package shipreq.webapp.base

import shipreq.base.util2.Util
import shipreq.base.prop._
import shipreq.webapp.base.prop._
import utest._
import Console.{RED, BOLD, WHITE_B, RESET}

object TestUtil {

  val jvm = try { "!" matches """\p{P}""" } catch { case _: Throwable => false }

  implicit val propSettings = (
    if (jvm) Settings(sampleSize = SampleSize(200))
    else     Settings(sampleSize = SampleSize(10))
    ).copy(debug = false, sizeDist = Seq(0.2 -> 0.2, 0.8 -> 0.8))

  def assertProp[A](p: Prop[A], g: Gen[A])(implicit S: Settings): Unit = {
    val r = PTest(p, g)(S)
    r match {
      case RunState(_, Satisfied) | RunState(_, Proved ) => ()
      case RunState(runs, Falsified(a)) =>
        if (!S.debug) {
          val v = a.toString
          val w = Util.escapeString(v)
          println(s"\n${RED}Falsified $WHITE_B[$p]$RESET$RED after $runs runs with:$RESET\n$v\n")
          if (w != v) println(s"$w\n")
        }
        assert(false)
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
