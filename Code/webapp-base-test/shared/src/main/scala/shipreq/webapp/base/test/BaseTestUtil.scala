package shipreq.webapp.base.test

import scala.io.AnsiColor._
import scalaz.Equal
import scalaz.syntax.equal._
import shipreq.base.util.Must
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.event._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.hash.HashScheme

object BaseTestUtil extends BaseTestUtil

trait BaseTestUtil {

  implicit class MustExtTest[A](m: Must[A]) {
    def unmust: A = m.fold(fail(_), identity)
  }

  def assertEq[A: Equal](actual: A, expect: A): Unit =
    assertEqO(None, actual, expect)

  def assertEq[A: Equal](name: => String, actual: A, expect: A): Unit =
    assertEqO(name.some, actual, expect)

  def assertEqO[A: Equal](name: => Option[String], actual: A, expect: A): Unit =
    if (actual ≠ expect) {
      println()
      def lead(s: String) = s"$RED_B$s$RESET "
      name.foreach(n => println(lead(">>>>>>>") + BOLD + YELLOW + n + RESET))

      val toString: Any => String = {
        case s: Stream[_] => s.force.toString() // SI-9266
        case a            => a.toString
      }

      var as = toString(actual)
      var es = toString(expect)
      var pre = "["
      var post = "]"
      if ((as + es) contains "\n") {
        pre = "↙[\n"
      }
      println(lead("expect:") + pre + BOLD + GREEN + es + RESET + post)
      println(lead("actual:") + pre + BOLD + RED + as + RESET + post)
      println()
      fail(s"assertEq${name.fold("")("(" + _ + ")")} failed.")
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
      fail("assertMultiline failed.")
    }

  def assertSet[A](actual: Set[A])(expect: A*): Unit = {
    val e = expect.toSet
    val missing = e -- actual
    val unexpected = actual -- e
    if (missing.nonEmpty || unexpected.nonEmpty)
      fail(s"Actual: $actual\nExpect: $e\n   Missing: $missing\nUnexpected: $unexpected")
  }

  def fail(msg: String, clearStackTrace: Boolean = true): Nothing =
    _fail(colourMultiline(msg, BOLD + MAGENTA), clearStackTrace)

  def _fail(msg: String, clearStackTrace: Boolean = true): Nothing = {
    val e = new AssertionError(msg)
    if (clearStackTrace)
      e.setStackTrace(Array.empty)
    throw e
  }

  private def colourMultiline(text: String, colour: String): String =
    colour + text.replace("\n", "\n" + colour) + RESET

  def assertContainsCI(actual: String, expectFrag: String): Unit =
    assertContains(actual.toLowerCase, expectFrag.toLowerCase)

  def assertContains(actual: String, expectFrag: String): Unit =
    if (!actual.contains(expectFrag)) {
      val a = colourMultiline(actual, BOLD + CYAN)
      _fail(s"${BOLD}${MAGENTA}Expected [${GREEN}$expectFrag${MAGENTA}] in:$RESET\n$a")
    }

  def verifyEvent(p: Project, e: Event): VerifiedEvent =
    _verifyEvent(p, e)._2

  def _verifyEvent(p: Project, e: Event): (Project, VerifiedEvent) = {
    val p2 = ApplyEvent.untrusted.apply1(e)(p).fold(sys.error, identity)
    val hs = HashScheme.latest
    val h = hs.hashProject hash p2
    (p2, VerifiedEvent(hs, h, e))
  }

  def verifyEvents(p0: Project)(es: Event*): VerifiedEvents = {
    var p = p0
    es.toVector.map { e =>
      val (p2, ve) = _verifyEvent(p, e)
      p = p2
      ve
    }
  }
}