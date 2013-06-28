package com.beardedlogic.usecase.lib

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import net.liftweb.util.Helpers._

class MiscTest extends FunSpec with ShouldMatchers with Misc {

  describe("#randomConfirmationToken") {
    it("should return different values each time") {
      randomConfirmationToken should not be(randomConfirmationToken)
      randomConfirmationToken should not be(randomConfirmationToken)
      randomConfirmationToken should not be(randomConfirmationToken)
    }
  }

  describe("#normaliseEmail") {
    it("should remove whitespace") {
      normaliseEmail("hehe") should be("hehe")
      normaliseEmail(" he  he ") should be("hehe")
    }
  }

  describe("#isEmailValid_?") {
    it("should require @") {
      isEmailValid_?("hehe@asd.com") should be(true)
      isEmailValid_?("heheasd.com") should be(false)
    }
    it("should not allow characters: &, <, >") {
      isEmailValid_?("h&ehe@asd.com") should be(false)
      isEmailValid_?("<hehe@asd.com") should be(false)
      isEmailValid_?(">hehe@asd.com") should be(false)
      isEmailValid_?("hehe@as&d.com") should be(false)
      isEmailValid_?("hehe@as<d.com") should be(false)
      isEmailValid_?("hehe@as>d.com") should be(false)
    }
  }

  describe("isConfirmationTokenExpired_?") {
    it("should consider 1-day-old valid") {
      isConfirmationTokenExpired_?(1.day.ago.toDate) should be(false)
    }
    it("should consider 1-week-old expired") {
      isConfirmationTokenExpired_?(1.week.ago.toDate) should be(true)
    }
  }
}
