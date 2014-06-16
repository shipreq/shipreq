package shipreq.webapp.feature.uc.step

import org.scalatest.Matchers
import org.scalatest.WordSpec

class StepLabelsTest extends WordSpec with Matchers {
  import StepLabels._

  "numeric" should {
    "turn 1 into 1" in { NUMERIC(1).value shouldEqual "1" }
    "turn 50 into 50" in { NUMERIC(50).value shouldEqual "50" }
  }

  "alpha" should {
    "turn 1 into a" in { ALPHA(1).value shouldEqual "a" }
    "turn 5 into e" in { ALPHA(5).value shouldEqual "e" }
    "turn 26 into z" in { ALPHA(26).value shouldEqual "z" }
    "turn 27 into aa" in { ALPHA(27).value shouldEqual "aa" }
    "turn 52 into az" in { ALPHA(52).value shouldEqual "az" }
    "turn 53 into ba" in { ALPHA(53).value shouldEqual "ba" }
  }

  "roman" should {
    "turn 1 into i" in { ROMAN(1).value shouldEqual "i" }
    "turn 2 into ii" in { ROMAN(2).value shouldEqual "ii" }
    "turn 3 into iii" in { ROMAN(3).value shouldEqual "iii" }
    "turn 4 into iv" in { ROMAN(4).value shouldEqual "iv" }
    "turn 5 into v" in { ROMAN(5).value shouldEqual "v" }
    "turn 6 into vi" in { ROMAN(6).value shouldEqual "vi" }
    "turn 7 into vii" in { ROMAN(7).value shouldEqual "vii" }
    "turn 8 into viii" in { ROMAN(8).value shouldEqual "viii" }
    "turn 9 into ix" in { ROMAN(9).value shouldEqual "ix" }
    "turn 10 into x" in { ROMAN(10).value shouldEqual "x" }
    "turn 11 into xi" in { ROMAN(11).value shouldEqual "xi" }
    "turn 14 into xiv" in { ROMAN(14).value shouldEqual "xiv" }
    "turn 19 into xix" in { ROMAN(19).value shouldEqual "xix" }
    "turn 20 into xx" in { ROMAN(20).value shouldEqual "xx" }
    "turn 38 into xxxviii" in { ROMAN(38).value shouldEqual "xxxviii" }
  }
}