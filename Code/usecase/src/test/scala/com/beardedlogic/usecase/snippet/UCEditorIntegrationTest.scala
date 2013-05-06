package com.beardedlogic.usecase.snippet

import com.beardedlogic.usecase.test.SeleniumDSL
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers

/**
 * Tests the use case editor.
 *
 * @since 29/04/2013
 */
class UCEditorIntegrationTest extends FreeSpec with ShouldMatchers with SeleniumDSL {

  "The editor page" - {
    "when first loaded" - {
      lazy val u = uce
      "should have a pre-populated UC ID" in { u.useCaseId should be("UC-1") }
      "should have an empty title" in { u.useCaseTitle should be("") }
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
            uce.setStepText(0, "what").setUseCaseTitle("noo").waitShort().stepText(0) should be("what")
          }
        }
      }
    }
  }

  "The step editor" ignore {
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
    "when pressed between 1.0 and 1.0.1" - {
      lazy val u = uce.setStepText(0, "NC").setStepText(1, "blah").clickAdd(0)
      "should not affect 1.0" in { u.assertStep(0)(0, "1.0", "NC") }
      "should renumber 1.0.1 to 1.0.2" in { u.assertStep(2)(1, "2", "blah") }
      "should add a new step: 1.0.1" in { u.assertStep(1)(1, "1", "") }
      "should add a new add button" in { u.assertAddButtonCount(3) }
    }

    "when pressed after 1.0.1" - {
      lazy val u = uce.setStepText(0, "NC").setStepText(1, "blah").clickAdd(1)
      "should not affect 1.0" in { u.assertStep(0)(0, "1.0", "NC") }
      "should not affect 1.0.1" in { u.assertStep(1)(1, "1", "blah") }
      "should add a new step: 1.0.2" in { u.assertStep(2)(1, "2", "") }
      "should add a new add button" in { u.assertAddButtonCount(3) }
    }
  }

  "The Delete button" - {
    "when page is first loaded" - {
      lazy val u = uce
      "should not be visible for 1.0" in { u.deleteButtonVisibility(0) should be(false) }
      "should be visible for 1.0.1" in { u.deleteButtonVisibility(1) should be(true) }
    }
    "when pressed for 1.0.2 (out of 1.0.3)" - {
      lazy val u = uce.clickAdd(1).clickAdd(1).assertStepCount(4)
        .setStepText(0 -> "head", 1 -> "pre", 2 -> "del", 3 -> "post").clickDelete(2)
      "should remove 1.0.2" in { u.assertStepCount(3) }
      "should not affect 1.0" in { u.assertStep(0)(0, "1.0", "head") }
      "should not affect 1.0.1" in { u.assertStep(1)(1, "1", "pre") }
      "should turn 1.0.3 into 1.0.2" in { u.assertStep(2)(1, "2", "post") }
    }
  }

  "The << button" - {
    "when page is first loaded" - {
      lazy val u = uce
      "should not be visible for 1.0" in { u.indentDecButtonVisibility(0) should be(false) }
      "should be visible for 1.0.1" in { u.indentDecButtonVisibility(1) should be(true) }
    }
    "when pressed for 1.0.2 (out of 1.0.3)" - {
      lazy val u = uce.clickAdd(1).clickAdd(1).assertStepCount(4).clickIndentDec(2)
      "should turn 1.0.2 into 1.1" in { u.assertStep(2)(0, "1.1") }
      "should turn 1.0.3 into 1.1.1" in { u.assertStep(3)(1, "1") }
      "should not be visible for 1.0.2" in { u.indentDecButtonVisibility(2) should be(false) }
      "should be visible for 1.0.3" in { u.indentDecButtonVisibility(3) should be(true) }
    }
    "when pressed for 1.0.2 then 1.0.1 (out of 1.0.3)" - {
      lazy val u = uce.clickAdd(1).clickAdd(1).assertStepCount(4).clickIndentDec(2).clickIndentDec(1)
      "should leave 1.0 as is" in { u.assertStep(0)(0, "1.0") }
      "should turn 1.0.1 into 1.1" in { u.assertStep(1)(0, "1.1") }
      "should turn 1.1 into 1.2" in { u.assertStep(2)(0, "1.2") }
      "should turn 1.1.1 into 1.2.1" in { u.assertStep(3)(1, "1") }
      "should not be visible for 1.1" in { u.indentDecButtonVisibility(1) should be(false) }
      "should not be visible for 1.2" in { u.indentDecButtonVisibility(2) should be(false) }
      "should be visible for 1.0.3" in { u.indentDecButtonVisibility(3) should be(true) }
    }
  }

  "The >> button" - {
    "when page is first loaded" - {
      lazy val u = uce
      "should not be visible for 1.0" in { u.indentIncButtonVisibility(0) should be(false) }
      "should be visible for 1.0.1" in { u.indentIncButtonVisibility(1) should be(false) }
    }
    "when pressed for 1.0.2 (out of 1.0.3)" - {
      lazy val u = uce.clickAdd(1).clickAdd(1).assertStepCount(4).clickIndentInc(2)
      "should turn 1.0.2 into 1.0.1.a" in { u.assertStep(2)(2, "a") }
      "should turn 1.0.3 into 1.0.2" in { u.assertStep(3)(1, "2") }
      "should not be visible for 1.0.1.a" in { u.indentIncButtonVisibility(2) should be(false) }
      "should be visible for 1.0.2" in { u.indentIncButtonVisibility(3) should be(true) }
    }
    "when pressed for 1.0.3 then 1.0.2 (out of 1.0.3)" - {
      lazy val u = uce.clickAdd(1).clickAdd(1).assertStepCount(4).clickIndentInc(3).clickIndentInc(2)
      "should leave 1.0 as is" in { u.assertStep(0)(0, "1.0") }
      "should leave 1.0.1 as is" in { u.assertStep(1)(1, "1") }
      "should turn 1.0.2 into 1.0.1.a" in { u.assertStep(2)(2, "a") }
      "should turn 1.0.3 into 1.0.1.a.i" in { u.assertStep(3)(3, "i") }
      "should not be visible for 1.0" in { u.indentIncButtonVisibility(0) should be(false) }
      "should not be visible for 1.0.1" in { u.indentIncButtonVisibility(1) should be(false) }
      "should not be visible for 1.0.1.a" in { u.indentIncButtonVisibility(2) should be(false) }
      "should not be visible for 1.0.1.a.i" in { u.indentIncButtonVisibility(3) should be(false) }
    }
  }
}
