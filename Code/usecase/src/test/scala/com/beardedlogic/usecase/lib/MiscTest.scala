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

  describe("isConfirmationTokenExpired_?") {
    it("should consider 1-day-old valid") {
      isConfirmationTokenExpired_?(1.day.ago) should be(false)
    }
    it("should consider 1-week-old expired") {
      isConfirmationTokenExpired_?(1.week.ago) should be(true)
    }
  }
}
