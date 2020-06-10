package shipreq.webapp.base.util

import utest._
import japgolly.microlibs.testutil.TestUtil._

object ReorderTest extends TestSuite {

  override def tests = Tests {
    val vec12345 = Vector(1, 2, 3, 4, 5)

    "move" - {
      def test(from: Int, to: Int, expect: Vector[Int]): Unit =
        assertEq(Reorder.usingUnivEq(from, to)(vec12345), expect)

      "downMid" - test(1, 4, Vector(2, 3, 4, 1, 5))
      "upMid"   - test(5, 3, Vector(1, 2, 5, 3, 4))
      "downEnd" - test(1, 5, Vector(2, 3, 4, 5, 1))
      "upEnd"   - test(5, 1, Vector(5, 1, 2, 3, 4))
      "midL"    - test(3, 2, Vector(1, 3, 2, 4, 5))
      "midR"    - test(3, 4, Vector(1, 2, 4, 3, 5))

      "nopTop" - test(1, 1, vec12345)
      "nopMid" - test(3, 3, vec12345)
      "nopBot" - test(5, 5, vec12345)
    }
  }
}
