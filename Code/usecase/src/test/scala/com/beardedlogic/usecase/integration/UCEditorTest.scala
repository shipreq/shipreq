package com.beardedlogic.usecase.integration

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.GivenWhenThen
import org.scalatest.FunSpec

/**
 * Tests the use case editor.
 *
 * @since 29/04/2013
 */
class UCEditorTest extends FunSpec with ShouldMatchers with SeleniumDSL with GivenWhenThen {

  describe("The editor page when first loaded") {
    lazy val u = uce.expectDelays(false)
    it("should have a pre-populated UC ID") { u.useCaseId should be("UC-1") }
    it("should have a title of 'Untitled'") { u.useCaseTitle should be("Untitled") }
    it("should have 2 steps") { u.stepCount should be(2) }
    it("should have a step: 1.0") { u.assertStep(0)(0, "1.0", "") }
    it("should have a step: 1.0.1") { u.assertStep(1)(1, "1", "") }
    it("should have 2 Add buttons") { u.addButtonCount should be(2) }
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe("A change to the use case title") {
    describe("when 1.0 is empty") {
      it("should set 1.0 to the UC title") {
        uce.setUseCaseTitle("hehe cool").assertStepText(0, "hehe cool")
      }
    }
    describe("when 1.0 was matched manually") {
      it("should set 1.0 to the UC title") {
        uce.setStepText(0, "override").setUseCaseTitle("hehe cool").setStepText(0, "hehe cool")
          .setUseCaseTitle("noo").assertStepText(0, "noo")
      }
    }
    describe("when 1.0 was matched automatically") {
      it("should set 1.0 to the UC title") {
        uce.setUseCaseTitle("hehe cool").assertStepText(0, "hehe cool")
          .setUseCaseTitle("noo").assertStepText(0, "noo")
      }
    }
    describe("when 1.0 is overridden") {
      it("should not affect 1.0") {
        uce.setStepText(0, "what").setUseCaseTitle("noo").assertStepText(0, "what")
      }
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

}