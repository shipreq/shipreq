package shipreq.webapp.client.project.widgets

import utest._
import shipreq.webapp.base.test._

object FilterEditorTest extends TestSuite {

  override def tests = Tests {
    "autoComplete" - {
      import shipreq.webapp.base.test.AutoCompleteTestUtil._
      implicit val strategies = FilterEditor.autoCompleteStrategies(SampleProject3.project)

      "field" - {

        "all" - assertSuggestionsFor("field:")(
          "\"All Tags\"",
          "Description",
          "\"Exception Courses\"",
          "\"Major Feature\"",
          "\"Normal and Alternate Courses\"",
          "Notes",
          "\"Other Tags\"",
          "Priority",
          "Released",
          "Reporter",
        )

        "simple" - assertSuggestionsAndSelectionFor("field:pri")("Priority")("field:Priority")

      }

    }
  }
}
