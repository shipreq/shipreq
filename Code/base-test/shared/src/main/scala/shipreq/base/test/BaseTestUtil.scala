package shipreq.base.test

import cats.{Eq, Order}
import java.time.{Duration, Instant}
import pprint.PPrinter
import shipreq.base.util.{Debug, NonEmptyArraySeq}

object BaseTestUtil extends BaseTestEquality with BaseTestUtil {

  final class BaseTestUtilOpsAny[A](private val a: A) extends AnyVal {
    def assertEq(expect: A)(implicit e: Eq[A]): Unit =
      BaseTestUtil.assertEq(a, expect)

    def assertEqN(name: => String, expect: A)(implicit e: Eq[A]): Unit =
      BaseTestUtil.assertEq(name, a, expect)
  }

  final class FieldAssert[A](actual: A, expect: A) {
    def assertEq[B: Eq](f: A => B) = {
      BaseTestUtil.assertEq(f(actual), f(expect))
      this
    }
    def assertEq[B: Eq](name: => String, f: A => B) = {
      BaseTestUtil.assertEq(name, f(actual), f(expect))
      this
    }
  }

  final class BaseTestUtilOpsOption[A](private val self: Option[A]) extends AnyVal {
    def getOrThrow(moreInfo: => String): A =
      self.getOrElse(fail(s"Option is empty: ${moreInfo.replaceFirst("\\.?$", ".")}"))
  }

  final class BaseTestUtilOpsSeq[A](private val self: Seq[A]) extends AnyVal {
    def sole(): A =
      self.length match {
        case 1 => self.head
        case n => fail(s"Expected one element, found $n: $self")
      }

    def asOption(): Option[A] =
      self.length match {
        case 0 => None
        case 1 => Some(self.head)
        case n => fail(s"Expected one element, found $n: $self")
      }
  }

  private val PrettyPrinter: PPrinter =
    pprint.copy(
      defaultWidth = 1,
      defaultHeight = 2000,
      additionalHandlers = {
        case x: NonEmptyArraySeq[Any] => pprint.treeify(x.whole, escapeUnicode = true, showFieldNames = false)
      }
    )
}

trait BaseTestUtil
  extends japgolly.microlibs.testutil.TestUtilWithoutUnivEq
  with Debug.Implicits {

  implicit def BaseTestUtilOpsAny[A](a: A) =
    new BaseTestUtil.BaseTestUtilOpsAny(a)

  implicit def BaseTestUtilOpsOption[A](a: Option[A]) =
    new BaseTestUtil.BaseTestUtilOpsOption(a)

  implicit def BaseTestUtilOpsSeq[A](a: Seq[A]) =
    new BaseTestUtil.BaseTestUtilOpsSeq(a)

  def pp: PPrinter =
    BaseTestUtil.PrettyPrinter

  def forceUnivEqOrderByToString[A]: Order[A] with UnivEq[A] = {
    val o = Order.by((_: A).toString)
    new Order[A] with UnivEq[A] {
      override def compare(a: A, b: A) = o.compare(a, b)
    }
  }

  def time[A](a: => A): (A, Duration) = {
    val start = Instant.now()
    val result = a
    val end = Instant.now()
    val dur = Duration.between(start, end)
    (result, dur)
  }

  def once[A](a: => A): () => A = {
    lazy val v = a
    () => v
  }

  def onceUnit[A](a: => A): () => Unit =
    once { a; () }

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
