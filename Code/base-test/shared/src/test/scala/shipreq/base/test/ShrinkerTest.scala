package shipreq.base.test

import utest._
import shipreq.base.util.Valid

object ShrinkerTest extends TestSuite {
  import BaseTestUtil._

  override def tests = Tests {

    "vectorRemoveOne" - {
      def validity(as: Vector[Int]) = Valid.when(as.sum < 100)
      val result = Shrink(Vector(1, 3, 5, 8, 70, 3, 80))(Shrinker.vectorRemoveOne, _.length, validity)
      assertEq(result, Vector(70, 80))
    }

  }
}
