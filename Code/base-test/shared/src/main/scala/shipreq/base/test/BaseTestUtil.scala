package shipreq.base.test

import scala.io.AnsiColor._
import scalaz.{Equal, Order}
import scalaz.std.string.stringInstance
import shipreq.base.util.UnivEq
import shipreq.base.util.ScalaExt._

object BaseTestUtil extends BaseTestEquality with BaseTestUtil {

  class BaseTestUtilOpsAny[A](private val a: A) extends AnyVal {
    def assertEq(expect: A)(implicit e: Equal[A]): Unit =
      BaseTestUtil.assertEq(a, expect)

    def assertEqN(name: => String, expect: A)(implicit e: Equal[A]): Unit =
      BaseTestUtil.assertEq(name, a, expect)
  }
}

trait BaseTestUtil extends scalaz.syntax.ToEqualOps {

  implicit def BaseTestUtilOpsAny[A](a: A) =
    new BaseTestUtil.BaseTestUtilOpsAny(a)

  def forceUnivEqOrderByToString[A]: Order[A] with UnivEq[A] =
    UnivEq.withOrder(Order.orderBy(_.toString))

  def assertEq[A: Equal](actual: A, expect: A): Unit =
    assertEqO(None, actual, expect)

  def assertEq[A: Equal](name: => String, actual: A, expect: A): Unit =
    assertEqO(name.some, actual, expect)

  private def lead(s: String) = s"$RED_B$s$RESET "
  private def failureStart(name: Option[String], leadSize: Int): Unit = {
    println()
    name.foreach(n => println(lead(">" * leadSize) + BOLD + YELLOW + n + RESET))
  }

  def assertEqO[A: Equal](name: => Option[String], actual: A, expect: A): Unit =
    if (actual ≠ expect) {
      failureStart(name, 7)

      val toString: Any => String = {
        case s: Stream[_] => s.force.toString() // SI-9266
        case a            => a.toString
      }

      var as = toString(actual)
      var es = toString(expect)
      val ss = as :: es :: Nil
      var pre = "["
      var post = "]"
      val htChars = ss.flatMap(s => s.headOption :: s.lastOption :: Nil)
      if (htChars.forall(_.exists(c => !Character.isWhitespace(c)))) {
        pre = ""
        post = ""
      }
      if (ss.exists(_ contains "\n")) {
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

  def assertSet[A](actual: Set[A])(expect: A*): Unit = assertSet(actual, expect.toSet)
  def assertSet[A](actual: Set[A], expect: Set[A]): Unit = assertSetO(None, actual, expect)
  def assertSet[A](name: => String, actual: Set[A])(expect: A*): Unit = assertSet(name, actual, expect.toSet)
  def assertSet[A](name: => String, actual: Set[A], expect: Set[A]): Unit = assertSetO(Some(name), actual, expect)

  def assertSetO[A](name: => Option[String], actual: Set[A], expect: Set[A]): Unit =
    if (actual != expect) {
      val missing = expect -- actual
      val unexpected = actual -- expect

      val leadSize = 9
      //if (missing.nonEmpty || unexpected.nonEmpty)
      //fail(s"Actual: $actual\nExpect: $expect\n   Missing: $missing\nUnexpected: $unexpected")
      def show(title: String, col: String, s: Set[A]): Unit =
        if (s.nonEmpty) {
          //val x = if (s.size == 1) s.head.toString else s.mkString("{ ",", "," }")
          val x = s.iterator.map(_.toString).toVector.sorted.mkString("\n" + (" " * (leadSize + 1)))
          println(lead(title) + col + x + RESET)
        }

      failureStart(name, leadSize)
      show(" missing:", BOLD + CYAN, missing)
      show("unwanted:", BOLD + RED, unexpected)
      println()
      fail(s"assertSet${name.fold("")("(" + _ + ")")} failed.")
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
}