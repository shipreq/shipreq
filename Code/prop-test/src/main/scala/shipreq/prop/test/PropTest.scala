package shipreq.prop.test

import shipreq.prop.util.Util
import shipreq.prop._
import Console.{RED, RED_B, WHITE_B, RESET, YELLOW}

object PropTest extends PropTestOps {
  implicit def defaultPropSettings = DefaultSettings.propSettings
}

object PropTestOps extends PropTestOps

trait PropTestOps {

  def testProp[A](p: Prop[A], g: Gen[A])(implicit S: Settings): Unit =
    assertSuccessful(p, PTest.test(p, g, S))

  def proveProp[A](p: Prop[A], d: Domain[A])(implicit S: Settings): Unit = {
    val rs = PTest.prove(p, d, S)
    assertSuccessful(p, rs)
    rs match {
      case RunState(n, Satisfied) =>
        println(s"${YELLOW}Proposition $RESET[$p]$YELLOW was satisfied but not proved after $RED$n runs$YELLOW of an expected ${d.size}.")
        fail(s"Proposition [$p] was satisfied but not proved after $n runs of an expected ${d.size}.")
      case _ => ()
    }
  }

  private[this] def assertSuccessful[A](p: Prop[A], rs: RunState[A])(implicit S: Settings): Unit =
    rs match {
      case RunState(_, Satisfied) | RunState(_, Proved) => ()

      case RunState(runs, Falsified(a, f)) =>
        failinfo(a, v => s"\n${RED}Falsified $WHITE_B[$p]$RESET$RED after $runs runs with:$RESET\n$v\n",
          println(f.report))
        fail(s"Failed: $p")

      case RunState(runs, Error(a, e)) =>
        failinfo(a, v => s"\n${RED_B}Crashed $WHITE_B$RED[$p]$RESET$RED_B after $runs runs with:$RESET\n$v\n",
          e.printStackTrace())
        fail(s"Failed: $p", e)
    }

  private[this] def failinfo[A](a: A, header: String => String, printFooter: => Unit)(implicit S: Settings): Unit = {
    if (!S.debug) {
      val v = a.toString
      val w = Util.escapeString(v)
      println(header(v))
      if (w != v) println(s"$w\n")
    }
    printFooter
    println()
  }

  private[this] def fail(s: String, e: Throwable = null): Nothing =
    throw new java.lang.AssertionError(s, e)

  implicit def autoToOpsPropExt  [A](p: Prop[A])   = new PropExt(p)
  implicit def autoToOpsGenExt   [A](p: Gen[A])    = new GenExt(p)
  implicit def autoToOpsDomainExt[A](p: Domain[A]) = new DomainExt(p)
}

import PropTestOps._

class PropExt[A](val p: Prop[A]) extends AnyVal {
  def mustBeSatisfiedBy         (g: Gen[A])(implicit S: Settings) = testProp(p, g)
  def mustBeSatisfiedBy_[B <: A](g: Gen[B])(implicit S: Settings) = testProp(p, g.subst[A])

  def mustBeProvedBy         (d: Domain[A])(implicit S: Settings) = proveProp(p, d)
  def mustBeProvedBy_[B <: A](d: Domain[B])(implicit S: Settings) = proveProp(p, d.subst[A])
}

class GenExt[A](val g: Gen[A]) extends AnyVal {
  def mustSatisfy         (p: Prop[A])(implicit S: Settings) = testProp(p, g)
  def _mustSatisfy[B >: A](p: Prop[B])(implicit S: Settings) = testProp(p, g.subst[B])
}

class DomainExt[A](val d: Domain[A]) extends AnyVal {
  def mustProve         (p: Prop[A])(implicit S: Settings) = proveProp(p, d)
  def _mustProve[B >: A](p: Prop[B])(implicit S: Settings) = proveProp(p, d.subst[B])
}
