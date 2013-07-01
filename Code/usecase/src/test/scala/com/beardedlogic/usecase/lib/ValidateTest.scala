package com.beardedlogic.usecase.lib

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import Validate._

class ValidateTest extends FunSpec with ShouldMatchers with Misc {

  describe("#email") {
    it("should require @") {
      email("hehe@asd.com") should be(true)
      email("heheasd.com") should be(false)
    }
    it("should require a . after @") {
      email("hehe@asdcom") should be(false)
      email("hehe@asd.com") should be(true)
    }
    it("should not allow characters: &, <, >") {
      email("h&ehe@asd.com") should be(false)
      email("<hehe@asd.com") should be(false)
      email(">hehe@asd.com") should be(false)
      email("hehe@as&d.com") should be(false)
      email("hehe@as<d.com") should be(false)
      email("hehe@as>d.com") should be(false)
    }
  }
}
