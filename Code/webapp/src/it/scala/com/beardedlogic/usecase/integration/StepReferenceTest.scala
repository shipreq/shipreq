package com.beardedlogic.usecase.integration

import org.openqa.selenium.By
import org.openqa.selenium.WebElement
import org.scalatest.BeforeAndAfter
import org.scalatest.FunSpec
import support._
import SeleniumDSL._

/**
 * Tests that references to steps are updated when the steps' labels/positions change.
 *
 * @since 10/05/2013
 */
class StepReferenceTest  extends FunSpec with SeleniumTest with BeforeAndAfter {

  var u: UseCaseEditorDSL = null
  var labelMap: Map[String, Int] = null

  after {
    u = null
    labelMap = null
  }

  def referring = selenium.findElement(By.cssSelector(".fieldFrame textarea"))

  def refText(stepLabel: String) = s"Look at [${stepLabel}]. Good."

  def mapSteps(stepNames: String*) {
    var i = -1
    labelMap = Map(stepNames.map { n => i += 1; n -> i }: _*)
    u.assertStepCount(stepNames.size)
  }

  // -------------------------------------------------------------------------------------------------------------------

  def init = {
    u = goto.useCaseEditor // load page
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
    And("there's a reference to " + stepLabel)
    referring.typeInto(refText(stepLabel) + "\t")
  }

  def when_type(stepLabel: String, text: String) { When(stepLabel + " has text: " + text); u.setStepText(labelMap(stepLabel), text + "\t") }
  def when_>>(stepLabel: String) { When(stepLabel + " is indented >>"); u.click_>>(labelMap(stepLabel)) }
  def when_<<(stepLabel: String) { When(stepLabel + " is indented <<"); u.click_<<(labelMap(stepLabel)) }
  def when_+(stepLabel: String) { When("new row inserted after " + stepLabel); u.clickAdd(labelMap(stepLabel)) }
  def when_-(stepLabel: String) { When(stepLabel + " is deleted"); u.clickDelete(labelMap(stepLabel)) }

  def then_ref_should_be(newStepLabel: String) {
    Then("the reference should be " + newStepLabel)
    eventually { referring.value should be(refText(newStepLabel)) }
  }

  def then_step_should_be(stepLabel: String, expectedText: String) = step_should_be("Then", stepLabel, expectedText)
  def _and_step_should_be(stepLabel: String, expectedText: String) = step_should_be("And", stepLabel, expectedText)
  def step_should_be(prefix: String, stepLabel: String, expectedText: String) {
    info(s"$prefix $stepLabel should have text: $expectedText")
    u.assertStepText(labelMap(stepLabel), expectedText)
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

      it("deletion") {
        given_101_exists; and_a_ref_to("1.0.1")
        when_-("1.0.1")
        then_ref_should_be("DELETED")
      }
    }

    describe("Invalid step ref links") {
      it("should be transformed after editing") {
        u = goto.useCaseEditor
        referring.typeInto(refText("1.0.5") + "\t")
        eventually { referring.value should be(refText("1.0.5?")) }
      }
    }
  }

  def refsAreParsed(setup: => Any, field: => WebElement, fieldAfterInsert: => WebElement = null) = {
    it("should transform on edit") {
      u = goto.useCaseEditor
      setup
      field.typeInto("I am [  1.0.1]\t")
      eventually { field.value should be("I am [1.0.1]") }
    }

    it("should update as refs change") {
      u = goto.useCaseEditor
      setup
      field.typeInto("I like [1.0.1]\t")
      u.clickAdd(0)
      eventually {
        var f = fieldAfterInsert
        if (f == null) f = field
        f.value should be("I like [1.0.2]")
      }
    }
  }

  describe("NC fields (existing)") {
    it should behave like refsAreParsed({}, u.stepTextElem(0))
  }

  describe("NC fields (new)") {
    it should behave like refsAreParsed(u.clickAdd(1).assertStepCount(3), u.stepTextElem(1), u.stepTextElem(2))
  }

  describe("EC fields") {
    it should behave like refsAreParsed(u.ec.clickAddTailStepButton.assertStepCount(1), u.ec.stepTextElem(0))
  }

  // TODO test multiple step refs in same field
  // TODO test step refs in multiple fields

  describe("Flow refs") {
    they("should generally work") {
      given_103_exists
      when_type("1.0.3", "-->1.0.1")
      then_step_should_be("1.0.3", "➡ [1.0.1]")
      _and_step_should_be("1.0.1", "⬅ [1.0.3]")

      when_type("1.0.2", "--> 1.0.1, 1.0.3")
      then_step_should_be("1.0.2", "➡ [1.0.1] [1.0.3]")
      _and_step_should_be("1.0.1", "⬅ [1.0.2] [1.0.3]")
      _and_step_should_be("1.0.3", "⬅ [1.0.2] ➡ [1.0.1]")

      when_<<("1.0.3"); mapSteps("1.0", "1.0.1", "1.0.2", "1.1")
      then_step_should_be("1.0.1", "⬅ [1.0.2] [1.1]")
      _and_step_should_be("1.0.2", "➡ [1.0.1] [1.1]")
      _and_step_should_be("1.1", "⬅ [1.0.2] ➡ [1.0.1]")

      when_-("1.0.2"); mapSteps("1.0", "1.0.1", "1.1")
      then_step_should_be("1.0.1", "⬅ [1.1]")
      _and_step_should_be("1.1", "➡ [1.0.1]")
    }
  }
}
