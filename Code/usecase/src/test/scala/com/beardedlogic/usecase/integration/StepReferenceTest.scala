package com.beardedlogic.usecase.integration

import com.beardedlogic.usecase.test.SeleniumDSL
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.GivenWhenThen
import org.scalatest.FunSpec
import org.scalatest.BeforeAndAfter
import org.openqa.selenium.WebElement
import org.openqa.selenium.By
import com.beardedlogic.usecase.test.TestHelpers

/**
 * Tests that references to steps are updated when the steps' labels/positions change.
 *
 * @since 10/05/2013
 */
class StepReferenceTest
  extends FunSpec
          with ShouldMatchers
          with SeleniumDSL
          with TestHelpers
          with BeforeAndAfter
          with GivenWhenThen {

  import SeleniumDSL._

  var u: UCEditorDSL = null
  var labelMap: Map[String, Int] = null

  after {
    u = null
    labelMap = null
  }

  def referring = s.findElement(By.cssSelector(".ucdata textarea"))

  def refText(stepLabel: String) = s"Look at [${stepLabel}]. Good."

  def mapSteps(stepNames: String*) {
    var i = -1
    labelMap = Map(stepNames.map { n => i += 1; n -> i }: _*)
    u.assertStepCount(stepNames.size)
  }

  // -------------------------------------------------------------------------------------------------------------------

  def init =  {
    u = uce // load page
    mapSteps("1.0", "1.0.1")
    u
  }

  def given_101_exists {
    Given("1.0 ~ 1.0.1 exist")
    init
  }

  def given_101a_exists {
    Given("1.0 ~ 1.0.1.a exist")
    init.addButtons(1).click_>>(2)
    mapSteps("1.0", "1.0.1", "1.0.1.a")
  }

  def given_102_exists {
    Given("1.0 ~ 1.0.2 exist")
    init.clickAdd(1)
    mapSteps("1.0", "1.0.1", "1.0.2")
  }

  def given_102a_exists {
    Given("1.0 ~ 1.0.2.a exist")
    init.addButtons(2).click_>>(3)
    mapSteps("1.0", "1.0.1", "1.0.2", "1.0.2.a")
  }

  def given_103_exists {
    Given("1.0 ~ 1.0.3 exist")
    init.addButtons(2)
    mapSteps("1.0", "1.0.1", "1.0.2", "1.0.3")
  }

  def and_a_ref_to(stepLabel: String) {
    And("a there's a reference to " + stepLabel)
    referring.typeInto(refText(stepLabel) + "\t")
  }

  def when_>>(stepLabel: String) { When(stepLabel + " is indented >>"); u.click_>>(labelMap(stepLabel)) }
  def when_<<(stepLabel: String) { When(stepLabel + " is indented <<"); u.click_<<(labelMap(stepLabel)) }
  def when_+(stepLabel: String) { When("new row inserted after " + stepLabel); u.clickAdd(labelMap(stepLabel)) }
  def when_-(stepLabel: String) { When(stepLabel + " is deleted"); u.clickDelete(labelMap(stepLabel)) }

  def then_ref_should_be(newStepLabel: String) {
    Then("the reference should be " + newStepLabel)
    eventually { referring.value should be(refText(newStepLabel)) }
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe("Valid step ref links") {
    describe("should be updated when the ref target's label changes due to...") {

      it("direct >>") {
        given_102_exists; and_a_ref_to("1.0.2")
        when_>>("1.0.2")
        then_ref_should_be("1.0.1.a")
      }

      it("direct <<") {
        given_101_exists; and_a_ref_to("1.0.1")
        when_<<("1.0.1")
        then_ref_should_be("1.1")
      }

      it("indirect >> of sibling") {
        given_103_exists; and_a_ref_to("1.0.3")
        when_>>("1.0.2") // 1.0.2 --> 1.0.1.a
        then_ref_should_be("1.0.2")
      }

      it("indirect << of sibling") {
        given_102_exists; and_a_ref_to("1.0.2")
        when_<<("1.0.1") // 1.1 <-- 1.0.1
        then_ref_should_be("1.1.1")
      }

      it("indirect >> of parent") {
        given_102a_exists; and_a_ref_to("1.0.2.a")
        when_>>("1.0.2") // -> 1.0.1.a
        then_ref_should_be("1.0.1.a.i")
      }

      it("indirect << of parent") {
        given_101a_exists; and_a_ref_to("1.0.1.a")
        when_<<("1.0.1") // 1.1 <-- 1.0.1
        then_ref_should_be("1.1.1")
      }

      it("insert before") {
        given_101_exists; and_a_ref_to("1.0.1")
        when_+("1.0")
        then_ref_should_be("1.0.2")
      }

      it("delete before") {
        given_102_exists; and_a_ref_to("1.0.2")
        when_-("1.0.1")
        then_ref_should_be("1.0.1")
      }
    }

    describe("should not change when different steps' labels change due to...") {
      it(">> after") { pending }
      it("<< after") { pending }
      it("irrelevant >> before") { pending }
      it("irrelevant << before") { pending }
      it("insert after") { pending }
      it("delete after") { pending }
    }

    // should ??? when ref deleted

    // should change when referenced in
    // - text
    // - NC
    // - AC
    // - EC
  }

  describe("Invalid step ref links") {
    // shouldn't be allowed to exist --> should be transformed on save
    // [1.0.4] --> [?1.0.4?]
  }

  describe("Multiple step ref links") {
    // in same field
    // in different fields
  }
}