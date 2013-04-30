package com.beardedlogic.usecase.snippet

import org.scalatest.{ Finders, WordSpec }
import org.scalatest.matchers.ShouldMatchers

class UCEditorTest extends WordSpec with ShouldMatchers {
  import UCEditor._

  "flattenNodes() " should {
    "flatten recursively" in {

      val c1_0_2_x =
        StepNode(2, "a", "1.0.2.a", Nil) ::
          StepNode(2, "b", "1.0.2.b", Nil) ::
          Nil

      val c1_0_x =
        StepNode(1, "1", "1.0.1", Nil) ::
          StepNode(1, "2", "1.0.2", c1_0_2_x) ::
          StepNode(1, "3", "1.0.3", Nil) ::
          Nil

      val c1_2_x =
        StepNode(1, "1", "1.2.1", Nil) ::
          Nil

      val top =
        StepNode(0, "1.0", "1.0", c1_0_x) ::
          StepNode(0, "1.1", "1.1", Nil) ::
          StepNode(0, "1.1", "1.2", c1_2_x) ::
          Nil

      flattenNodes(top).map(_.id) should be(List(
        "1.0", "1.0.1", "1.0.2", "1.0.2.a", "1.0.2.b", "1.0.3",
        "1.1",
        "1.2", "1.2.1"))
    }
  }
}