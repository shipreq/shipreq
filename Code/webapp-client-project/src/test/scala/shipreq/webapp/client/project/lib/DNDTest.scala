package shipreq.webapp.client.project.lib

import scalaz.std.AllInstances._
import shipreq.webapp.client.project.test.TestUtil._
import utest._

object DNDTest extends TestSuite {
  val vec12345 = Vector(1, 2, 3, 4, 5)

  override def tests = Tests {
    'move {
      def test(from: Int, to: Int, expect: Vector[Int]): Unit =
        assertEq(DND.moveE(from, to)(vec12345), expect)

      'downMid - test(1, 4, Vector(2, 3, 4, 1, 5))
      'upMid   - test(5, 3, Vector(1, 2, 5, 3, 4))
      'downEnd - test(1, 5, Vector(2, 3, 4, 5, 1))
      'upEnd   - test(5, 1, Vector(5, 1, 2, 3, 4))
      'midL    - test(3, 2, Vector(1, 3, 2, 4, 5))
      'midR    - test(3, 4, Vector(1, 2, 4, 3, 5))

      'nopTop - test(1, 1, vec12345)
      'nopMid - test(3, 3, vec12345)
      'nopBot - test(5, 5, vec12345)
    }
  }
}
