package shipreq.webapp.client.util

import scalaz.std.AllInstances._
import shipreq.webapp.client.test.TestUtil._
import utest._

object DNDTest extends TestSuite {

  override def tests = TestSuite {
    'move {
      def test(from: Int, to: Int, expect: Vector[Int]): Unit =
        assertEq(DND.move(from, to)(Vector(1, 2, 3, 4, 5)), expect)

      'downMid - test(1, 4, Vector(2, 3, 4, 1, 5))
      'upMid   - test(5, 3, Vector(1, 2, 5, 3, 4))
      'downEnd - test(1, 5, Vector(2, 3, 4, 5, 1))
      'upEnd   - test(5, 1, Vector(5, 1, 2, 3, 4))
      'midL    - test(3, 2, Vector(1, 3, 2, 4, 5))
      'midR    - test(3, 4, Vector(1, 2, 4, 3, 5))
    }
  }
}
