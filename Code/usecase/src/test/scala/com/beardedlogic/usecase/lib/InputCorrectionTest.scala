package com.beardedlogic.usecase.lib

import org.scalatest.FunSpec
import org.scalatest.Matchers
import InputCorrection._

class InputCorrectionTest extends FunSpec with Matchers with Misc {

  describe("#email") {
    it("should remove whitespace") {
      email("hehe") should be("hehe")
      email(" he  he ") should be("hehe")
    }
  }

  describe("#username") {
    it("should trim & make lowercase") {
      username(" Hehe ") should be("hehe")
    }
  }
}
