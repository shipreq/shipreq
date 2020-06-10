package shipreq.base.test

import utest._
import shipreq.base.util.FxModule._
import shipreq.base.util.Valid

object ShrinkerFxTest extends TestSuite {
  import BaseTestUtil._

  override def tests = Tests {

    "vectorRemoveOne" - {
      def validity(as: Vector[Int]) = Valid.when(as.sum < 100)
      val result = ShrinkFx(Vector(1, 3, 5, 8, 70, 3, 80))(Shrinker.vectorRemoveOne, _.length, x => Fx.pure(validity(x))).unsafeRunSync()
      assertEq(result, Vector(70, 80))
    }

  }
}
