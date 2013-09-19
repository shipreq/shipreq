package com.beardedlogic.usecase.lib

import org.scalatest.Matchers
import org.scalatest.WordSpec

class StepLabelsTest extends WordSpec with Matchers {
  import StepLabels._

  "numeric" should {
    "turn 1 into 1" in { NUMERIC(1) should be("1") }
    "turn 50 into 50" in { NUMERIC(50) should be("50") }
  }

  "alpha" should {
    "turn 1 into a" in { ALPHA(1) should be("a") }
    "turn 5 into e" in { ALPHA(5) should be("e") }
    "turn 26 into z" in { ALPHA(26) should be("z") }
    "turn 27 into aa" in { ALPHA(27) should be("aa") }
    "turn 52 into az" in { ALPHA(52) should be("az") }
    "turn 53 into ba" in { ALPHA(53) should be("ba") }
  }

  "roman" should {
    "turn 1 into i" in { ROMAN(1) should be("i") }
    "turn 2 into ii" in { ROMAN(2) should be("ii") }
    "turn 3 into iii" in { ROMAN(3) should be("iii") }
    "turn 4 into iv" in { ROMAN(4) should be("iv") }
    "turn 5 into v" in { ROMAN(5) should be("v") }
    "turn 6 into vi" in { ROMAN(6) should be("vi") }
    "turn 7 into vii" in { ROMAN(7) should be("vii") }
    "turn 8 into viii" in { ROMAN(8) should be("viii") }
    "turn 9 into ix" in { ROMAN(9) should be("ix") }
    "turn 10 into x" in { ROMAN(10) should be("x") }
    "turn 11 into xi" in { ROMAN(11) should be("xi") }
    "turn 14 into xiv" in { ROMAN(14) should be("xiv") }
    "turn 19 into xix" in { ROMAN(19) should be("xix") }
    "turn 20 into xx" in { ROMAN(20) should be("xx") }
    "turn 38 into xxxviii" in { ROMAN(38) should be("xxxviii") }
  }
}