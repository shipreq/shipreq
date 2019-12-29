package shipreq.base.test

import japgolly.microlibs.testutil.TestUtilInternals
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.time.{Duration, Instant}
import scalaz.std.string.stringInstance
import scalaz.{Equal, Order, \/}
import sourcecode.Line
import shipreq.base.util.Identity
import shipreq.base.util.univeq._

object BaseTestUtil extends BaseTestEquality with BaseTestUtil {

  final class BaseTestUtilOpsAny[A](private val a: A) extends AnyVal {
    def assertEq(expect: A)(implicit e: Equal[A]): Unit =
      BaseTestUtil.assertEq(a, expect)

    def assertEqN(name: => String, expect: A)(implicit e: Equal[A]): Unit =
      BaseTestUtil.assertEq(name, a, expect)
  }

  final class BaseTestUtilOpsEither[A, B](private val e: Either[A, B]) extends AnyVal {
    def needLeft(implicit l: Line): A =
      e.fold(Identity.apply, e => fail(s"needLeft got Right($e)"))
    def needRight(implicit l: Line): B =
      e.fold(e => fail(s"needRight got Left($e)"), Identity.apply)
  }

  final class BaseTestUtilOpsDisj[A, B](private val d: A \/ B) extends AnyVal {
    def needLeft(implicit l: Line): A =
      d.fold(Identity.apply, e => fail(s"needLeft got \\/-($e)"))
    def needRight(implicit l: Line): B =
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
  extends japgolly.microlibs.testutil.TestUtilWithoutUnivEq
  with scalaz.syntax.ToEqualOps {

  implicit def BaseTestUtilOpsAny[A](a: A) =
    new BaseTestUtil.BaseTestUtilOpsAny(a)

  implicit def BaseTestUtilOpsDisj[A, B](d: A \/ B) =
    new BaseTestUtil.BaseTestUtilOpsDisj(d)

  implicit def BaseTestUtilOpsEither[A, B](e: Either[A, B]) =
    new BaseTestUtil.BaseTestUtilOpsEither(e)

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

  // TODO move into microlibs
  def equalInstantWithTolerance(tolerance: Duration): Equal[Instant] =
    Equal((a, b) => {
      val d = Duration.between(b, a).abs()
      tolerance.compareTo(d) > 0
    })

  // TODO Move getOrThrow() into microlibs
  // TODO Move these into microlibs(jvm-only)

  import scala.io.{Codec, Source}

  def writeFile(filename: String, content: String): Unit =
    Files.write(Paths get filename, content getBytes StandardCharsets.UTF_8)

  def readFile(filename: String): String = {
    val src = Source.fromFile(filename)(Codec.UTF8)
    try src.mkString finally src.close()
  }

  def readResourceFile(filename: String): String = {
    val src = Source.fromResource(filename)(Codec.UTF8)
    try src.mkString finally src.close()
  }

}