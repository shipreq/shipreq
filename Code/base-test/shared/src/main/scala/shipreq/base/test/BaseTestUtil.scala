package shipreq.base.test

import scalaz.std.string.stringInstance
import scalaz.{Equal, Order, \/}
import shipreq.base.util.Identity
import shipreq.base.util.univeq._

object BaseTestUtil extends BaseTestEquality with BaseTestUtil {

  final class BaseTestUtilOpsAny[A](private val a: A) extends AnyVal {
    def assertEq(expect: A)(implicit e: Equal[A]): Unit =
      BaseTestUtil.assertEq(a, expect)

    def assertEqN(name: => String, expect: A)(implicit e: Equal[A]): Unit =
      BaseTestUtil.assertEq(name, a, expect)
  }

  final class BaseTestUtilOpsDisj[A, B](private val d: A \/ B) extends AnyVal {
    def needLeft: A =
      d.fold(Identity.apply, sys error _.toString)
    def needRight: B =
      d.fold(sys error _.toString, Identity.apply)
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
  extends japgolly.microlibs.testutil.TestUtil
  with scalaz.syntax.ToEqualOps {

  implicit def BaseTestUtilOpsAny[A](a: A) =
    new BaseTestUtil.BaseTestUtilOpsAny(a)

  implicit def BaseTestUtilOpsDisj[A, B](d: A \/ B) =
    new BaseTestUtil.BaseTestUtilOpsDisj(d)

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

  def assertFields[A](actual: A, expect: A) =
    new BaseTestUtil.FieldAssert(actual, expect)

//  def assertMatch[A](a: A)(pf: PartialFunction[A, Unit]): Unit =
//    if (!pf.isDefinedAt(a))
//      fail(s"Wrong shape: $a")

  // TODO Move into microlibs
  def assertChange[A, B: Equal, R](query: => A, block: => R)(actual: (A, A) => B)(expect: (A, R) => B): R =
    assertChangeO(None, query, block)(actual)(expect)

  def assertChange[A, B: Equal, R](desc: => String, query: => A, block: => R)(actual: (A, A) => B)(expect: (A, R) => B): R =
    assertChangeO(Some(desc), query, block)(actual)(expect)

  def assertChangeO[A, B: Equal, R](desc: => Option[String], query: => A, block: => R)(actual: (A, A) => B)(expect: (A, R) => B): R = {
    val before = query
    val result = block
    val after  = query
    assertEqO(desc, actual(after, before), expect(before, result))
    result
  }

  def assertNoChange[B : Equal, A](query: => B)(block: => A): A =
    assertNoChangeO(None, query)(block)

  def assertNoChange[B : Equal, A](desc: => String, query: => B)(block: => A): A =
    assertNoChangeO(Some(desc), query)(block)

  def assertNoChangeO[B : Equal, A](desc: => Option[String], query: => B)(block: => A): A =
    assertChangeO(desc, query, block)((b, _) => b)((b, _) => b)

  def assertDifference[N: Numeric : Equal, A](query: => N)(expect: N)(block: => A): A =
    assertDifferenceO(None, query)(expect)(block)

  def assertDifference[N: Numeric : Equal, A](desc: => String, query: => N)(expect: N)(block: => A): A =
    assertDifferenceO(Some(desc), query)(expect)(block)

  def assertDifferenceO[N: Numeric : Equal, A](desc: => Option[String], query: => N)(expect: N)(block: => A): A =
    assertChangeO(desc, query, block)(implicitly[Numeric[N]].minus)((_, _) => expect)
}