package shipreq.webapp
package feature.uc.text

import org.scalatest.FunSpec
import test.TestHelpers
import ParsingUtils._

class ParsingUtilsTest extends FunSpec with TestHelpers {

  implicit val ss = StepState1

  describe("areLabelsValid_?") {

    it("should return true when input is empty") {
      areLabelsValid_?(List.empty) should be(true)
    }
    it("should return true when all labels match") {
      areLabelsValid_?(List(S1)) should be(true)
      areLabelsValid_?(List(S1, S1)) should be(true)
      areLabelsValid_?(List(S1, S1, S5)) should be(true)
    }
    it("should return false when a label doesnt match") {
      areLabelsValid_?(List(SF)) should be(false)
      areLabelsValid_?(List(S1, S1, SF)) should be(false)
    }
  }
}
