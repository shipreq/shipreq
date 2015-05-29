package shipreq.webapp.base.test

import shipreq.base.util.Must
import shipreq.base.util.ScalaExt._
import scalaz.Equal
import scalaz.syntax.equal._

object BaseTestUtil extends BaseTestUtil

trait BaseTestUtil {

  implicit class MustExtTest[A](m: Must[A]) {
    def unmust: A = m.fold(fail, identity)
  }

  def assertEq[A: Equal](actual: A, expect: A): Unit =
    assertEqO(None, actual, expect)

  def assertEq[A: Equal](name: => String, actual: A, expect: A): Unit =
    assertEqO(name.some, actual, expect)

  def assertEqO[A: Equal](name: => Option[String], actual: A, expect: A): Unit =
    if (actual ≠ expect) {
      println()
      name.foreach(n => println(s">>>>>>> $n"))
      val as = actual.toString
      val es = expect.toString
      if ((as + es) contains "\n")
        println(s"actual: ↙[\n$as]\nexpect: ↙[\n$es]")
      else
        println(s"actual: [$as]\nexpect: [$es]")
      println()
      assert(false)
    }

  def assertMultiline(actual: String, expect: String): Unit =
    if (actual != expect) {
      println()
      val AE = List(actual, expect).map(_.split("\n"))
      val List(as, es) = AE
      val lim = as.length max es.length
      val List(maxA,maxE) = AE.map(x => (0 #:: x.map(_.length).toStream).max)
      val maxL = lim.toString.length
      println("A|E")
      val fmt = s"%${maxL}d: %-${maxA}s |%s| %s\n"
      for (i <- (0 until lim)) {
        val List(a, e) = AE.map(s => if (i >= s.length) "" else s(i))
        val cmp = if (a == e) " " else "≠"
        printf(fmt, i + 1, a, cmp, e)
      }
      println()
      assert(false)
    }

  def assertSet[A](actual: Set[A])(expect: A*): Unit = {
    val e = expect.toSet
    val missing = e -- actual
    val unexpected = actual -- e
    if (missing.nonEmpty || unexpected.nonEmpty)
      fail(s"Actual: $actual\nExpect: $e\n   Missing: $missing\nUnexpected: $unexpected")
  }

  def fail(msg: String): Nothing =
    throw new AssertionError(msg)
}