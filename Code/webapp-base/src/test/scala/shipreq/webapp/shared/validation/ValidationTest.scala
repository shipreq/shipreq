package shipreq.webapp.shared.validation

import utest._
import GenericValidators._

object ValidationTest extends TestSuite {
  override def tests = TestSuite {

    val v = largeText("test")

    def test(i: String, expect: String): Unit = {
      val actual = v.correctU(i)
      assert(actual == expect)
    }

    'symbols {
      test("q>=w && z<=3", "q≥w && z≤3")
      test("q >= w && z <= 3", "q ≥ w && z ≤ 3")
      test(">=w && z<=", "≥w && z≤")
      test("a <=> b", "a <=> b")
      test("a _=> b", "a _=> b")
      test("a >= b >= c", "a ≥ b ≥ c")
    }
  }
}
