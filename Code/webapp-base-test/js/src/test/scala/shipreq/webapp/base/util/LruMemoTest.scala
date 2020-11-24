package shipreq.webapp.base.util

import shipreq.base.test.BaseTestUtil._
import sourcecode.Line
import utest._

object LruMemoTest extends TestSuite {

  override def tests = Tests {

    "1" - {
      var calls = 0

      val f = {
        val g: Int => Int =
          i => {
            calls += 1
            i + 1
          }
        LruMemo(g, 3).byReusability
      }

      def test(i: Int, expectedCalls: Int)(implicit l: Line): Unit = {
        val a = f(i)
        assertEq(s"$a / $calls", s"${i + 1} / $expectedCalls")
      }

      test(3, 1) // 3
      test(3, 1) // 3
      test(4, 2) // 4 3
      test(3, 2) // 3 4
      test(0, 3) // 0 3 4
      test(2, 4) // 2 0 3
      test(0, 4) // 0 2 3
      test(4, 5) // 4 0 2
      test(3, 6) // 3 4 0
      test(0, 6) // 0 3 4
      test(5, 7) // 5 0 3
      test(0, 7) // 0 5 3
      test(3, 7) // 3 0 5
      test(5, 7) // 5 3 0
      test(6, 8) // 6 5 3
      test(3, 8) // 3 6 5
    }

  }
}
