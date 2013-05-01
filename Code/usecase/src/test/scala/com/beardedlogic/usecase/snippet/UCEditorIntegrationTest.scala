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
}
