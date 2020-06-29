package shipreq.base.test

import shipreq.base.util.Valid
import utest._

object ShrinkerTest extends TestSuite {
  import BaseTestUtil._

  override def tests = Tests {

    "vectorRemoveOne" - {
      def validity(as: Vector[Int]) = Valid.when(as.sum < 100)
      val result = Shrink(Vector(1, 3, 5, 8, 70, 3, 80))(Shrinker.removeElements, _.length, validity)
      assertEq(result, Vector(70, 80))
    }

  }
}
