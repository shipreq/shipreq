package shipreq.webapp.base.util

import shipreq.base.test.BaseTestUtil._
import sourcecode.Line
import utest._

object LastValueMemoTest extends TestSuite {

  override def tests = Tests {
    "tuple2" - {
      var r1 = 0
      var r2 = 0
      val c1 = LastValueMemo{ (i: Int) => r1 += 1; i.toString}
      val c2 = LastValueMemo{ (i: Int) => r2 += 1; i.toString}
      val c  = LastValueMemo.apply2(c1, c2)(_ + ", " + _)

      def test(a: Int, b: Int)(runs1: Int, runs2: Int)(implicit l: Line) = {
        val x = c((a, b))
        assertEq((x, r1, r2), (s"$a, $b", runs1, runs2))
      }

      assertEq((r1, r2), (0, 0))
      test(3, 4)(1, 1)
      test(3, 4)(1, 1)
      test(4, 4)(2, 1)
      test(4, 3)(2, 2)
      test(4, 3)(2, 2)
      test(3, 4)(3, 3)
    }
  }
}
