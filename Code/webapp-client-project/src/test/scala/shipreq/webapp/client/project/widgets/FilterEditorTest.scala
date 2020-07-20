package shipreq.webapp.client.project.widgets

import shipreq.webapp.base.test._
import utest._

object FilterEditorTest extends TestSuite {

  override def tests = Tests {
    "autoComplete" - {
      import shipreq.webapp.base.test.AutoCompleteTestUtil._
      implicit val strategies = FilterEditor.autoCompleteStrategies(SampleProject3.project)

      "field" - {

        "all" - assertSuggestionsFor("field:")(
          "All Tags",
          "Description",
          "Exception Courses",
          "Major Feature",
          "Normal and Alternate Courses",
          "Notes",
          "Other Tags",
          "Priority",
          "Released",
          "Reporter",
        )

        "simple"   - assertSuggestionsAndSelectionFor("field:pri")("Priority")("field:Priority")
        "middle"   - assertSuggestionsAndSelectionFor("field:tes")("Notes")("field:Notes")
        "quotes"   - assertSuggestionsAndSelectionFor("field:OTH")("Other Tags")("field:\"Other Tags\"")
        "impField" - assertSuggestionsAndSelectionFor("field:maj")("Major Feature")("field:MF")
      }

      "fieldAttr" - {
        "basic"   - assertSuggestionsAndSelectionFor("field:MF=")("blank", "default", "n/a", "notBlank")("field:MF=blank")
        "quoted"  - assertSuggestionsAndSelectionFor("field:\"ALL TAGS\"=")("blank", "default", "n/a", "notBlank")("field:\"ALL TAGS\"=blank")
        "partial" - assertSuggestionsAndSelectionFor("field:mf=DEF")("default")("field:mf=default")
      }

    }
  }
}
