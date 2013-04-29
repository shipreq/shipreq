package com.beardedlogic.usecase.integration

import com.beardedlogic.usecase.test.{Jetty, SeleniumTestSupport, TestHelpers}
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.JavaConversions._

/**
 * Tests the use case editor.
 *
 * @since 29/04/2013
 */
class UCEditorTest extends WordSpec with ShouldMatchers with SeleniumTestSupport with TestHelpers {

  "The Use Case Editor" should {

    "start blank" in {
      s.get(Jetty.URL)
      s.findElementById("total_steps").getText should be("2")
    }

    "add new steps" in {
      s.get(Jetty.URL)
      s.findElementsByCssSelector("input[value=Add]")(0).click
      expectSoon { s.findElementById("total_steps").getText should be("3") }
    }

  }
}