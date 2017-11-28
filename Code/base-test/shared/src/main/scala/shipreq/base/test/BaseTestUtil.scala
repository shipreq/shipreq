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
      d.fold(Identity.apply, e => fail(s"needLeft got \\/-($e)"))
    def needRight: B =
      d.fold(e => fail(s"needRight got -\\/($e)"), Identity.apply)
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
}