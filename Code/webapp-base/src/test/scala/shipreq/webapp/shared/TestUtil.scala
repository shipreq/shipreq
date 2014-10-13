package shipreq.webapp.shared

import shipreq.base.util.Util
import shipreq.webapp.shared.prop._
import utest._

object TestUtil {

  val js = try { java.awt.Color.BLACK == null } catch { case _: Throwable => true }

  implicit val propSettings = (
    if (js) Settings(sampleSize = SampleSize(4))
    else    Settings(sampleSize = SampleSize(100))
    ).copy(debug = false)

  def assertProp[A](p: Prop[A], g: Gen[A])(implicit S: Settings): Unit = {
    val r = PTest(p, g)(S)
    r match {
      case RunState(_, Satisfied) | RunState(_, Proved ) => ()
      case RunState(runs, Falsified(a)) =>
        if (!S.debug) {
          val v = a.toString
          val w = Util.escapeString(v)
          println(s"\nFalsified after $runs runs with:\n$v\n")
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
