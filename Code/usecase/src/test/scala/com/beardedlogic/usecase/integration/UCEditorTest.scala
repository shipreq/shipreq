package com.beardedlogic.usecase.integration

import com.beardedlogic.usecase.test.SeleniumDSL
import org.scalatest.{ Finders, FreeSpec }
import org.scalatest.matchers.ShouldMatchers

/**
 * Tests the use case editor.
 *
 * @since 29/04/2013
 */
class UCEditorTest extends FreeSpec with ShouldMatchers with SeleniumDSL {

  "The editor page" - {
    "when first loaded" - {
      "should have a pre-populated UC ID" in { uce.useCaseId should be("UC-1") }
      "should have an empty title" in { uce.useCaseTitle should be("") }
      "should have 2 steps in total" in { uce.stepCount should be(2) }
      "should have a top-level step" - {
        "which is blank" in { uce.stepText(0) should be("") }
        "which is numbered 1.0" in { uce.stepPosition(0) should be("1.0.") }
        "which has no parent" in pending
      }
      "should have a second-level step" - {
        "which is blank" in { uce.stepText(1) should be("") }
        "which is numbered 1" in { uce.stepPosition(1) should be("1.") }
        "which is a child of 1.0" in pending
      }
      "should have 2 add-step buttons in total" in pending
      "should have an add-step button between 1.0 and 1.0.1" in pending
      "should have an add-step button after 1.0.1" in pending
    }
  }

  "The use case title" - {
    "when edited" - {
      "should cause the normal-course step text" - {
        "to match the use case title" - {
          "when previously empty" in pending
          "when previously matched" in pending
        }
        "not to change" - {
          "when previously overridden" in pending
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
}
