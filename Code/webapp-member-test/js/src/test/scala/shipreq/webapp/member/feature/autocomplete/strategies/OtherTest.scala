package shipreq.webapp.member.feature.autocomplete.strategies

import shipreq.webapp.member.project.data._
import shipreq.webapp.member.test.AutoCompleteTestUtil._
import shipreq.webapp.member.test.project.SampleProject3._
import utest._

object OtherTest extends TestSuite {
  implicit val labelSel = SuggestionLabelSel("div div:first-child")

  override def tests = Tests {

    // AI generated test case.
    // Ensures that when two candidate generators produce the same candidate, it only appears once in the suggestions.
    // Eg. "BUG-1: Broken buttons" should only appear once on "bu", even though it matches "BUG-1" and "button".
    "duplicate" - {
      implicit val autoCompleteStyle = ReqItem.Style.IdAndTitle
      val items = ProjectStrategies.reqItems(project, plainText)
      val item = items.head

      val c1: RefStrategies.Candidates = _ => Iterator(item.candidate)
      val c2: RefStrategies.Candidates = _ => Iterator(item.candidate)

      val combined = RefStrategies.combineCandidates(c1, c2)
      val strategies = RefStrategies(combined).apply(shipreq.webapp.member.project.data.Contextualise)

      withAutoComplete(strategies) { implicit ctx =>
        ctx.suggest("[" + item.pubidStr)
        val labels = suggestions().map(_.label)
        if (labels.size != 1) {
          println(s"\nDEBUG: labels = $labels\n")
        }
        assert(labels.size == 1)
      }
    }

    // Auto-completing on "deferred" was resulting in "analysed ddeferred"
    "badInsertion" - {
      implicit val strategies: Strategies =
        HashtagStrategies(List(HashRefKey("deferred")))(Plain)

      "startOfString" - {
        assertSuggestionsAndSelectionFor("d")("deferred")("deferred")
      }

      "onSpace" - {
        assertSuggestionsAndSelectionFor("analysed ")("deferred")("analysed deferred")
      }

      "afterSpace1" - {
        assertSuggestionsAndSelectionFor("analysed d")("deferred")("analysed deferred")
      }

      "afterSpace2" - {
        assertSuggestionsAndSelectionFor("analysed de")("deferred")("analysed deferred")
      }
    }

  }
}
