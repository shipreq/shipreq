package shipreq.base.util

import japgolly.microlibs.testutil.TestUtil._
import utest._

object EitherStateTest extends TestSuite {

  override def tests = Tests {

    "stackSafety" - {
      val n = 200000

      val Eval = EitherState.ForTypes[Int, Unit]
      import Eval.eitherStateUnderlyingMonad

      val inc    = Eval.mod(_ + 1)
      val prog   = List.fill(n)(inc).iterator.reduce(_ >> _)
      val result = prog.exec(0)

      assertEq(result, \/-(n))
    }
  }
}
