package shipreq.webapp.util

import org.scalatest.{Matchers, FunSuite}
import scalaz.{NonEmptyList => NEL}

class ConciseIntListDescTest extends FunSuite with Matchers {

  test("Computation") {
    ConciseIntListDesc.compute(NEL(1,2,3,4,6,7,10,13,14,15))(identity) shouldBe "1-4, 6, 7, 10, 13-15"
  }
}
