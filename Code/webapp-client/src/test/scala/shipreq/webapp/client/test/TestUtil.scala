package shipreq.webapp.client.test

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import shipreq.webapp.base.protocol.Routine
import shipreq.webapp.client.lib.FailureIO
import shipreq.webapp.client.protocol.ClientProtocol
import scala.scalajs.js
import scalaz.Equal
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import scalaz.effect.IO
import shipreq.prop.test.Gen
import shipreq.base.util.Debug._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.client.lib.ui._
import RowStatus._

object TestUtil {

  def assertEq[A: Equal](actual: A, expect: A): Unit =
    assertEq(None, actual, expect)

  def assertEq[A: Equal](name: String, actual: A, expect: A): Unit =
    assertEq(name.some, actual, expect)

  def assertEq[A: Equal](name: Option[String], actual: A, expect: A): Unit =
    if (actual ≠ expect) {
      println()
      name.foreach(n => println(s">>>>>>> $n"))
      println(s"actual: [$actual]\nexpect: [$expect]")
      println()
      assert(false)
    }

  def assertRowStatusFailed(r: RowStatus): RowStatus.Failed =
    r match {
      case f@ RowStatus.Failed(_) => f
      case f => sys.error(s"Expected a failed row. Got: $f")
    }

  def fail(msg: String): Nothing =
    throw new AssertionError(msg)

  case class AB[A,B](a: A, b: B)

  def genAB[A, B](ga: Gen[A], gb: Gen[B]): Gen[AB[A,B]] =
    Gen.apply2(AB.apply[A, B])(ga, gb)

  type TestFields2[A, B] = FieldSet2[AB[A, B], A, B]

  def fields2[A,B](empty: (A,B)): TestFields2[A, B] =
    FieldSet2[AB[A,B]](_.a, _.b)(empty)

  implicit def eqCallbackEvent[B] = Equal.equalA[CallbackEvent[B]]

  implicit val eqRowStatus = Equal.equalA[RowStatus]

  val failedRowStatus =
    Failed(IO(()))

  def genRowStatus: Gen[RowStatus] =
    Gen.oneof(Sync, Locked, failedRowStatus)

  def sole[A](a: js.Array[A]): A = {
    assertEq(a.length, 1)
    a.head
  }

  implicit def autodomnode(c: ComponentScope_M[TopNode]) = c.getDOMNode()
}