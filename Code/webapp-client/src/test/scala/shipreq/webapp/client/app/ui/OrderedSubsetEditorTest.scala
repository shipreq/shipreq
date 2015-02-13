package shipreq.webapp.client.app.ui

import scalaz.Equal
import scalaz.std.anyVal._
import scalaz.std.vector._
import utest._
import shipreq.webapp.client.test.TestUtil._

object OrderedSubsetEditorTest extends TestSuite {

  def move(cur: Int*)(inactive: Int*)(mandatory: Int => Boolean) =
    OrderedSubsetEditor.move(cur.toVector, inactive, mandatory, Equal[Int]) _

  def expect(exp: Int*)(t: Vector[Int]): Unit =
    assertEq(t, exp.toVector)

  val `123|456` = move(1, 2, 3)(4, 5, 6)(_ => false)

  override def tests = TestSuite {

    "[1]23|456" - {
      // Moving down. Inserts after `to`
      * - expect(2, 1, 3)(`123|456`(1, 2)) // 2 * 3 | 4 5 6
      * - expect(2, 3, 1)(`123|456`(1, 3)) // 2 3 * | 4 5 6
      * - expect(2, 3   )(`123|456`(1, 4)) // 2 3 | 4 * 5 6
      * - expect(2, 3   )(`123|456`(1, 5)) // 2 3 | 4 5 * 6
      * - expect(2, 3   )(`123|456`(1, 6)) // 2 3 | 4 5 6 *
    }

    "123|45[6]" - {
      // Moving up. Inserts before `to`
      * - expect(6, 1, 2, 3)(`123|456`(6, 1)) // * 1 2 3 | 4 5
      * - expect(1, 6, 2, 3)(`123|456`(6, 2)) // 1 * 2 3 | 4 5
      * - expect(1, 2, 6, 3)(`123|456`(6, 3)) // 1 2 * 3 | 4 5
      * - expect(1, 2, 3, 6)(`123|456`(6, 4)) // 1 2 3 * | 4 5
      * - expect(1, 2, 3   )(`123|456`(6, 5)) // 1 2 3 | 4 * 5
    }

    'preventDraggingMandatoryOff {
      val test = move(1, 2, 3)(4, 5)(_ == 1)
      * - expect(1, 3   )(test(2, 5))
      * - expect(2, 1, 3)(test(1, 2)) // can still shuffle mandatory
      * - expect(1, 2, 3)(test(1, 5))
    }
  }
}
