package com.beardedlogic.usecase.lib

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import InputCorrection._

class InputCorrectionTest extends FunSpec with ShouldMatchers with Misc {

  describe("#email") {
    it("should remove whitespace") {
      email("hehe") should be("hehe")
      email(" he  he ") should be("hehe")
    }
  }
}
