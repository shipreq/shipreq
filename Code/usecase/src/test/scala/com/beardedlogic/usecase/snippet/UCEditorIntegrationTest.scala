package com.beardedlogic.usecase.snippet

import com.beardedlogic.usecase.test.SeleniumDSL
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.GivenWhenThen

/**
 * Tests the use case editor.
 *
 * @since 29/04/2013
 */
class UCEditorIntegrationTest extends FreeSpec with ShouldMatchers with SeleniumDSL with GivenWhenThen {

  /**
   * Turns an new UC into this:
   *
   * [0] 1.0
   * [1]   +-- 1
   * [2]   +-- 2
   * [3]   +-- 3
   */
  def startWith103 = uce.clickAdd(1).assertStepCount(3).clickAdd(2).assertStepCount(4)

  /**
   * Turns an new UC into this:
   *
   * [0] 1.0
   * [1]   +- 1
   * [2]   +- 2
   * [3]   |  +- a
   *       |
   * [4]   +- 3
   * [5]      +- a
   */
  def startWith102a3a = startWith103
    .clickAdd(3).assertStepCount(5)
    .clickAdd(4).assertStepCount(6)
    .clickIndentInc(3).assertStep(3)(2, "a")
    .clickIndentInc(5).assertStep(5)(2, "a")

  /**
   * Turns an new UC into this:
   *
   * [0] 1.0
   * [1]   +- 1
   * [2]   +- 2
   * [3]   |  +- a
   * [4]   |     +- i
   *       |
   * [5]   +- 3
   * [6]      +- a
   */
  def startWith102ai3a = startWith102a3a
    .clickAdd(3).assertStepCount(7)
    .clickIndentInc(4).assertStep(4)(3, "i")

  /**
   * Turns an new UC into this:
   *
   * [0] 1.0
   * [1]   +- 1
   * [2] 1.1
   * [3]   +- 1
   * [4]   |  +- a
   * [5]   +- 2
   * [6] 1.2
   * [7]   +- 1
   */
  def startWith_10_11x_12 = startWith103
    .clickAdd(3).assertStepCount(5)
    .clickAdd(4).assertStepCount(6)
    .clickAdd(5).assertStepCount(7)
    .clickAdd(6).assertStepCount(8)
    .clickIndentDec(2).assertStep(2)(0, "1.1")
    .clickIndentInc(4).assertStep(4)(2, "a")
    .clickIndentDec(6).assertStep(6)(0, "1.2")

  "The editor page" - {
    "when first loaded" - {
      lazy val u = uce.expectDelays(false)
      "should have a pre-populated UC ID" in { u.useCaseId should be("UC-1") }
      "should have a title of 'Untitled'" in { u.useCaseTitle should be("Untitled") }
      "should have 2 steps" in { u.stepCount should be(2) }
      "should have a step: 1.0" in { u.assertStep(0)(0, "1.0", "") }
      "should have a step: 1.0.1" in { u.assertStep(1)(1, "1", "") }
      "should have 2 Add buttons" in { u.addButtonCount should be(2) }
    }
  }

  "The use case title" - {
    "when edited" - {
      "should cause the normal-course step text" - {
        "to match the use case title" - {
          "when previously empty" in {
            uce.setUseCaseTitle("hehe cool").assertStepText(0, "hehe cool")
          }
          "when previously matched automatically" in {
            uce.setUseCaseTitle("hehe cool").assertStepText(0, "hehe cool")
              .setUseCaseTitle("noo").assertStepText(0, "noo")
          }
          "when previously matched manually" in {
            uce.setStepText(0, "override").setUseCaseTitle("hehe cool").setStepText(0, "hehe cool")
              .setUseCaseTitle("noo").assertStepText(0, "noo")
          }
        }
        "not to change" - {
          "when previously overridden" in {
            uce.setStepText(0, "what").setUseCaseTitle("noo").assertStepText(0, "what")
          }
        }
      }
    }
  }

  "The step editor" - {
    "when Enter is pressed" - {
      "and editing 1.0" - {
        "should not add a new step" in pending
        "should move focus to 1.0.1" in pending
      }
      "and editing 1.1" - {
        "should add a new step" in pending
        "should move focus to 1.0.2" in pending
      }
    }
  }

  "The Add button" - {
    "when pressed for 1.0" - {
      lazy val u = uce.setStepText(0, "NC").setStepText(1, "blah").clickAdd(0)
      "should not affect 1.0" in { u.assertStep(0)(0, "1.0", "NC") }
      "should renumber 1.0.1 to 1.0.2" in { u.assertStep(2)(1, "2", "blah") }
      "should add a new step: 1.0.1" in { u.assertStep(1)(1, "1", "") }
      "should add a new add button" in { u.assertAddButtonCount(3) }
    }

    "when pressed for 1.0.1" - {
      lazy val u = uce.setStepText(0, "NC").setStepText(1, "blah").clickAdd(1)
      "should not affect 1.0" in { u.assertStep(0)(0, "1.0", "NC") }
      "should not affect 1.0.1" in { u.assertStep(1)(1, "1", "blah") }
      "should add a new step: 1.0.2" in { u.assertStep(2)(1, "2", "") }
      "should add a new add button" in { u.assertAddButtonCount(3) }
    }

    "when pressed for 1.1" in {
      Given("1.1 exists"); val u = uce.clickIndentDec(1).assertStep(1)(0, "1.1")
      When("1.1's add button is clicked"); u.clickAdd(1)
      Then("it should create 1.1.1"); u.assertStep(2)(1, "1")
    }
  }

  "The Delete button" - {
    "when page is first loaded" - {
      lazy val u = uce
      "should not be visible for 1.0" in { u.deleteButtonVisibility(0) should be(false) }
      "should be visible for 1.0.1" in { u.deleteButtonVisibility(1) should be(true) }
    }

    "when pressed for 1.0.2 (out of 1.0.3)" - {
      lazy val u = startWith103.setStepText(0 -> "head", 1 -> "pre", 2 -> "del", 3 -> "post").clickDelete(2)
      "should remove 1.0.2" in { u.assertStepCount(3) }
      "should not affect 1.0" in { u.assertStep(0)(0, "1.0", "head") }
      "should not affect 1.0.1" in { u.assertStep(1)(1, "1", "pre") }
      "should turn 1.0.3 into 1.0.2" in { u.assertStep(2)(1, "2", "post") }
    }

    "should remove node's children too" in {
      Given("tree depths are 1.0.2.a.i and 1.0.3.a"); val u = startWith102ai3a.setStepText(5 -> "old 103", 6 -> "old 103a")
      When("1.0.2 is deleted"); u.clickDelete(2)
      Then("there should be 4 steps left"); u.assertStepCount(4)
      And("1.0.3 should now be 1.0.2"); u.assertStep(2)(1, "2", "old 103")
      And("1.0.3.a should now be 1.0.2.a"); u.assertStep(3)(2, "a", "old 103a")
    }

    "should not work for 1.0" in {
      uce.deleteButtonVisibility(0) should be(false)
    }

    "should work for 1.1" in {
      Given("A page with 1.1"); val u = uce.clickIndentDec(1).ac.assertStepCount(1)
      And("it's visible for 1.1"); u.deleteButtonVisibility(0) should be(true)
      When("clicked"); u.clickDelete(0)
      Then("1.1 should disappear"); u.assertStepCount(0)
      And("the addTailStep button remains"); u.assertHasAddTailStepButton
    }

    "should work for 1.E.1" in {
      Given("A page with 1.E.1"); val u = uce.ec.clickAddTailStepButton.assertStepCount(1)
      And("it's visible for 1.E.1"); u.deleteButtonVisibility(0) should be(true)
      When("clicked"); u.clickDelete(0)
      Then("1.E.1 should disappear"); u.assertStepCount(0)
      And("the addTailStep button remains"); u.assertHasAddTailStepButton
    }
  }

  "The << button" - {
    "when page is first loaded" - {
      lazy val u = uce.expectDelays(false)
      "should not be visible for 1.0" in { u.indentDecButtonVisibility(0) should be(false) }
      "should be visible for 1.0.1" in { u.indentDecButtonVisibility(1) should be(true) }
    }

    "when pressed for 1.0.1.a" in {
      val u = startWith103
        .clickIndentInc(2).assertStep(2)(2, "a") // 1.0.2 --> 1.0.1.a
        .clickIndentDec(2).assertStep(2)(1, "2") // 1.0.2 <-- 1.0.1.a
        .assertStep(3)(1, "3")
        .ac.assertStepCount(0)
    }

    "when pressed for 1.0.2 (without children)" in {
      val u = startWith103.clickIndentDec(2) // 1.1 <-- 1.0.2 moves down to AC
        .nc.assertStepCount(2) // NC should have 1.0 and 1.0.1
        .ac.assertStepCount(2) // AC should have 1.1 and 1.1.1
        .assertStep(0)(0, "1.1")
        .assertStep(1)(1, "1")
        .assertButtons(0, (false, true))
        .assertButtons(1, (true, false))
    }

    "when pressed for 1.0.2 (w/ children) then 1.1.2" in {
      val u = startWith102a3a

      u.clickIndentDec(2).assertStep(2)(0, "1.1") // 1.1 <-- 1.0.2 moves down to AC
        .nc.assertStepCount(2) // NC should have 1.0 and 1.0.1
        .ac.assertStepCount(4) // AC should have 1.1 and 1.1.[1-3]

      u.clickIndentDec(4).ac // 1.2 <-- 1.1.2
        .assertStep(0)(0, "1.1")
        .assertStep(1)(1, "1")
        .assertStep(2)(0, "1.2")
        .assertStep(3)(1, "1")
        .assertButtons(0, (false, true))
        .assertButtons(1, (true, false))
        .assertButtons(2, (false, true))
        .assertButtons(3, (true, false))
    }
  }

  "The >> button" - {
    "when page is first loaded" - {
      lazy val u = uce
      "should not be visible for 1.0" in { u.indentIncButtonVisibility(0) should be(false) }
      "should be visible for 1.0.1" in { u.indentIncButtonVisibility(1) should be(false) }
    }
    "when pressed for 1.0.2 (out of 1.0.3)" - {
      lazy val u = startWith103.clickIndentInc(2)
      "should turn 1.0.2 into 1.0.1.a" in { u.assertStep(2)(2, "a") }
      "should turn 1.0.3 into 1.0.2" in { u.assertStep(3)(1, "2") }
      "should not be visible for 1.0.1.a" in { u.indentIncButtonVisibility(2) should be(false) }
      "should be visible for 1.0.2" in { u.indentIncButtonVisibility(3) should be(true) }
    }
    "when pressed for 1.0.3 then 1.0.2 (out of 1.0.3)" - {
      lazy val u = startWith103.clickIndentInc(3).clickIndentInc(2)
      "should leave 1.0 as is" in { u.assertStep(0)(0, "1.0") }
      "should leave 1.0.1 as is" in { u.assertStep(1)(1, "1") }
      "should turn 1.0.2 into 1.0.1.a" in { u.assertStep(2)(2, "a") }
      "should turn 1.0.3 into 1.0.1.a.i" in { u.assertStep(3)(3, "i") }
      "should not be visible for 1.0" in { u.indentIncButtonVisibility(0) should be(false) }
      "should not be visible for 1.0.1" in { u.indentIncButtonVisibility(1) should be(false) }
      "should not be visible for 1.0.1.a" in { u.indentIncButtonVisibility(2) should be(false) }
      "should not be visible for 1.0.1.a.i" in { u.indentIncButtonVisibility(3) should be(false) }
    }

    // [0] 1.0
    // [1]   +- 1
    // [2]   +- 2
    // [3]      +- a
    // [4]      |  +- i
    // [5]      +- b
    // [6] 1.1
    // [7]   +- 1
    "when pressed for 1.1" in {
      val u = startWith_10_11x_12
        .clickIndentInc(2).assertStep(2)(1, "2") // 1.1 --> 1.0.2
        .nc.assertStepCount(6)
        .assertStep(3)(2, "a")
        .assertStep(4)(3, "i")
        .assertStep(5)(2, "b")
        .assertButtons(2, (true, true)) // 1.0.2
        .assertButtons(3, (true, false)) // 1.0.2.a
        .assertButtons(4, (true, false)) // 1.0.2.a.i
        .assertButtons(5, (true, true)) // 1.0.2.b
        .ac.assertStepCount(2) // AC should have 1.1 and 1.1.1
        .assertStep(0)(0, "1.1")
        .assertStep(1)(1, "1")
        .assertButtons(0, (false, true))
        .assertButtons(1, (true, false))
    }
  }

  "The Alternate Courses addTailStep button" - {
    "should be visible when there are no AC steps" in {
      Given("A page with no AC steps yet"); val u = uce.ac.assertStepCount(0)
      Then("it should be visible"); u.assertHasAddTailStepButton
    }

    "should create 1.1 first" in {
      Given("A page with no AC steps yet"); val u = uce.ac.assertStepCount(0)
      When("clicked"); u.clickAddTailStepButton
      Then("it should create 1.1"); u.assertStep(0)(0, "1.1")
      And("remain visible"); u.assertHasAddTailStepButton
    }

    "should create 1.2 when 1.1 exists" in {
      Given("A page with 1.1"); val u = uce.clickIndentDec(1).ac.assertStepCount(1)
      When("clicked"); u.clickAddTailStepButton
      Then("it should create 1.2"); u.assertStep(1)(0, "1.2")
      And("remain visible"); u.assertHasAddTailStepButton
    }
  }

  "The Exceptions addTailStep button" - {
    "should be visible when there are no exception steps" in {
      Given("A page with no exceptions yet"); val u = uce.ec.assertStepCount(0)
      Then("it should be visible"); u.assertHasAddTailStepButton
    }
    
    "should create 1.E.1 first" in {
      Given("A page with no exceptions yet"); val u = uce.ec.assertStepCount(0)
      When("clicked"); u.clickAddTailStepButton
      Then("it should create 1.E.1"); u.assertStep(0)(0, "1.E.1")
      And("remain visible"); u.assertHasAddTailStepButton
    }

    "should create 1.E.2 when 1.E.1 exists" in {
      Given("A page with 1.E.1"); val u = uce.ec.clickAddTailStepButton.assertStepCount(1)
      When("clicked"); u.clickAddTailStepButton
      Then("it should create 1.E.2"); u.assertStep(1)(0, "1.E.2")
      And("remain visible"); u.assertHasAddTailStepButton
    }
  }
}