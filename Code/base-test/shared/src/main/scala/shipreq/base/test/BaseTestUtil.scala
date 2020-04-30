package shipreq.base.test

import japgolly.microlibs.testutil.TestUtilInternals
import scalaz.std.string.stringInstance
import scalaz.{Equal, Order}
import shipreq.base.util.Debug
import shipreq.base.util.univeq._

object BaseTestUtil extends BaseTestEquality with BaseTestUtil {

  final class BaseTestUtilOpsAny[A](private val a: A) extends AnyVal {
    def assertEq(expect: A)(implicit e: Equal[A]): Unit =
      BaseTestUtil.assertEq(a, expect)

    def assertEqN(name: => String, expect: A)(implicit e: Equal[A]): Unit =
      BaseTestUtil.assertEq(name, a, expect)
  }

  final class FieldAssert[A](actual: A, expect: A) {
    def assertEq[B: Equal](f: A => B) = {
      BaseTestUtil.assertEq(f(actual), f(expect))
      this
    }
    def assertEq[B: Equal](name: => String, f: A => B) = {
      BaseTestUtil.assertEq(name, f(actual), f(expect))
      this
    }

  }
}

trait BaseTestUtil
  extends japgolly.microlibs.testutil.TestUtilWithoutUnivEq
  with Debug.Implicits
  with scalaz.syntax.ToEqualOps {

  implicit def BaseTestUtilOpsAny[A](a: A) =
    new BaseTestUtil.BaseTestUtilOpsAny(a)

  def forceUnivEqOrderByToString[A]: Order[A] with UnivEq[A] = {
    val o = Order.orderBy((_: A).toString)
    new Order[A] with UnivEq[A] {
      override def order(a: A, b: A) = o.order(a, b)
      override def equal(a: A, b: A) = a == b
    }
  }

  def once[A](a: => A): () => A = {
    lazy val v = a
    () => v
  }

  def onceUnit[A](a: => A): () => Unit =
    once { a; () }

  def quoteStringForDisplay(s: String) =
    TestUtilInternals.quoteStringForDisplay(s)

  def assertFields[A](actual: A, expect: A) =
    new BaseTestUtil.FieldAssert(actual, expect)

//  def assertMatch[A](a: A)(pf: PartialFunction[A, Unit]): Unit =
//    if (!pf.isDefinedAt(a))
//      fail(s"Wrong shape: $a")

  def shrinkUnequalStrings(str1: String, str2: String): (String, String) = {
    var a = str1
    var b = str2
    val minLen = str1.length min str2.length
    def mod(f: String => String): Unit = {
      a = f(a)
      b = f(b)
    }

    if (str1 == str2)
      return ("", "")

    for (n <- 0.until(minLen).dropWhile(i => str1(i) == str2(i)).headOption)
      if (n > 3)
        mod("…" + _.drop(n))

    for (n <- 1.to(minLen).dropWhile(i => str1(str1.length - i) == str2(str2.length - i)).headOption)
      if (n > 3)
        mod(_.dropRight(n) + "…")

    (a, b)
  }

  def shrinkUnequalStrings(str1: String, str2: String, limit: Int): (String, String) = {
    def f(s: String): String =
      if (s.length > limit)
        s.take(limit - 1) + "…"
      else
        s
    val (a, b) = shrinkUnequalStrings(str1, str2)
    (f(a), f(b))
  }

}