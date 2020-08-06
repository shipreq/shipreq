package shipreq.webapp.base.feature.autocomplete.strategies

import shipreq.webapp.base.feature.AutoCompleteFeature.AutoComplete.Strategies
import shipreq.webapp.base.test.AutoCompleteTestUtil._
import shipreq.webapp.base.text.Grammar
import utest._

object AutoCompleteTestModules {

  sealed abstract class Module() extends TestSuite {
    implicit val strategies: Strategies
  }

  abstract class Issue() extends Module {

    protected def `#D` = Seq("TBD", "TODO") // PENDING is dead

    final override def tests = Tests {
      "issue" - {
        "start"      - assertSuggestionsAndSelectionFor("#T")("TBD", "TODO")("#TBD")
        "mid"        - assertSuggestionsAndSelectionFor("#DO")("TODO")("#TODO")
        "noSyntax"   - assertSuggestionsFor("T")()
        "filterDead" - assertSuggestionsFor("#D")(`#D`: _*)
        "heading"    - assertSuggestionsFor("##")()
      }
    }
  }

  abstract class TagC() extends Module {
    final override def tests = Tests {
      "start"      - assertSuggestionsAndSelectionFor("#pri")("pri=high", "pri=low", "pri=med")("#pri=high")
      "mid"        - assertSuggestionsAndSelectionFor("#1")("v1.0", "v1.1", "v1.2", "v1.3", "v1.x")("#v1.0")
      "noSyntax"   - assertSuggestionsFor("pri")()
      "filterDead" - assertSuggestionsFor("#x")("v1.x", "v2.x") // v3.x & v4.x are dead
    }
  }

  abstract class TagP() extends Module {
    final override def tests = Tests {
      "start"      - assertSuggestionsAndSelectionFor("pri")("pri=high", "pri=low", "pri=med")("pri=high")
      "mid"        - assertSuggestionsAndSelectionFor("1")("v1.0", "v1.1", "v1.2", "v1.3", "v1.x")("v1.0")
      "withSyntax" - assertSuggestionsAndSelectionFor("#pri")("pri=high", "pri=low", "pri=med")("pri=high")
      "filterDead" - assertSuggestionsFor("#x")("v1.x", "v2.x") // v3.x & v4.x are dead
    }
  }

  abstract class ReqC() extends Module {
    final override def tests = Tests {
      implicit val labelSel = SuggestionLabelSel("div div:first-child")

      def assertSuggestsMFs(input: String)(exp: Int*)(implicit ctx: AutoCompleteTestCtx): Unit =
        assertSuggests(input)(exp.map("MF-" + _): _*)

      "pubid" - withAutoComplete(strategies) { implicit ctx =>
        assertSuggestsMFs("[mf1")(1, 10, 11, 12, 13, 14, 15, 16, 17, 18)
        assertSuggestsMFs("[mf8")(8)
        assertSuggestsMFs("[mf-8")(8)
        assertSuggests("[FR")("FR-1", "FR-2")
        assertSuggestsMFs("[14")(14)
        assertSelect("[MF-14:] ")
      }
      "title" - withAutoComplete(strategies) { implicit ctx =>
        assertSuggestsMFs("[save")(17)
        assertSuggestsMFs("[Collab")(9, 10, 11)
        assertSelect("[MF-9:] ")
      }
      "ignoreCase" - withAutoComplete(strategies) { implicit ctx =>
        assertSuggestsMFs("[require")(12, 13, 22, 23, 24)
        assertSuggestsMFs("[REQUIRE")(12, 13, 22, 23, 24)
      }
      "filterDeadByPubid" - assertSuggestionsFor("[mf28")()
      "filterDeadByTitle" - assertSuggestionsFor("[search")("MF-25") // excludes CO-{1,2}
      "complete" - assertSuggestionsFor("[MF-9]")()
      "afterText" - withAutoComplete(strategies) { implicit ctx =>
        assertSuggestsMFs("om\nfg [save")(17)
        assertSelect("om\nfg [MF-17:] ")
      }
    }
  }

  abstract class ReqCodePrefixes() extends Module {
    final override def tests = Tests {
      implicit val multilineData = List[(String, String)](
        ("hehe.no\n", ""),
        ("", "\nhehe.no"),
        ("omg.yay\nhehe.no\n", ""),
        ("", "\nomg.yay\nhehe.no"),
        ("omg.yay\n", "\nhehe.no"),
        ("more.again\nomg.yay\n", "\nhehe.no\nok.then"))

      "root" - withAutoComplete(strategies) { implicit ctx =>
        assertSuggests("a")("aaaa1", "abc", "amp", "apple", "apply")
        assertSelect("aaaa1")
        assertSuggests("ap")("apple", "apply")
        assertSuggests("b")("baa", "bcd")
        assertSuggests("2")("2a", "2b")
        assertSuggests("d")() // no dead.eggs
      }
      "soleExact" - withAutoComplete(strategies) { implicit ctx =>
        assertSuggests("1")()
        assertSuggests("c")("c", "cant")
      }
      "path" - withAutoComplete(strategies) { implicit ctx =>
        assertSuggests("abc.a")("aqua", "around", "art")
        assertSuggests("abc.ar")("around", "art")
        assertSuggests("abc.arou")("around")
        assertSuggests("abc.around.t")("tbc", "torn")
        assertSelect("abc.around.tbc")
      }
      "open" - withAutoComplete(strategies) { implicit ctx =>
        assertSuggests("abc.")("aqua", "around", "art", "bark")
        assertSuggests("abc.around.")("1", "2", "now", "tbc", "torn")
        assertSuggests("apply.")()
        assertSuggests("bcd.")("aaaz")
        assertSelect("bcd.aaaz")
      }
      "dot" - withAutoComplete(strategies) { implicit ctx =>
        assertSuggests("x"*100)() // Clear previous
        assertSuggests(".")()
      }
      "mid" - withAutoComplete(strategies) { implicit ctx =>
        assertSuggests(".eg")("goat.damn.egg", "goat.damn.egglike", "shit.eggs")
        assertSuggests(".egg")("goat.damn.egg", "goat.damn.egglike", "shit.eggs")
        assertSuggests(".eggs")("shit.eggs")
      }
      "multilinePrefix" - withAutoComplete(strategies) { implicit ctx => assertSuggestionsAndSelectionMultiline("ap")("apple", "apply")("apple") }
      "multilineMid"    - withAutoComplete(strategies) { implicit ctx => assertSuggestionsAndSelectionMultiline(".eggs")("shit.eggs")("shit.eggs") }
      "filterDead"      - assertSuggestionsFor("dea")()
    }
  }

  abstract class ReqCodeRefs() extends Module {

    protected def app = Seq("apple", "apply")

    final override def tests = Tests {
      implicit val labelSel =
        SuggestionLabelSel("div > div > span:first-child")

      "root" - withAutoComplete(strategies) { implicit ctx =>
        assertSuggests("[app")(app: _*)
        assertSelect("[apple] ")
        assertSuggests("[goa")("goat.damn.egg.crap", "goat.damn.egg.stuff", "goat.damn.egglike")
      }
      "path" - withAutoComplete(strategies) { implicit ctx =>
        val arounds = List("abc.around.1", "abc.around.2", "abc.around.now", "abc.around.tbc", "abc.around.torn")
        assertSuggests("[abc.arou")(arounds: _*)
        assertSelect("[abc.around.1] ")
        assertSuggests("[abc.round")(arounds: _*)
        assertSuggests("[a.round")(arounds: _*)
        assertSuggests("[c.round")(arounds: _*)
        assertSuggests("[g.d.e.s")("goat.damn.egg.stuff")
      }
      "skipNode" - withAutoComplete(strategies) { implicit ctx =>
        assertSuggests("[g.e.s")("goat.damn.egg.stuff")
        assertSuggests("[a.now")("abc.around.now")
        assertSuggests("[abc.t")("abc.around.tbc", "abc.around.torn", "abc.art")
        assertSuggests("[arou.t")("abc.around.tbc", "abc.around.torn")
      }
      "mid"        - assertSuggestionsFor("[aqu")("abc.aqua")
      "soleExact"  - assertSuggestionsFor("[aaaa1")("aaaa1")
      "dotStart"   - assertSuggestionsFor("[.egg")("goat.damn.egg.crap", "goat.damn.egg.stuff", "goat.damn.egglike", "shit.eggs")
      "dotEnd"     - assertSuggestionsFor("[egg.")("goat.damn.egg.crap", "goat.damn.egg.stuff")
      "filterDead" - assertSuggestionsFor("[dea")()
    }
  }

  abstract class Tex() extends Module {
    final override def tests = Tests {
      "tex" - withAutoComplete(strategies) { implicit ctx =>
        assertSuggests("<t")(Grammar.texTag)
        assertSelect(s"<${Grammar.texTag}>|</${Grammar.texTag}>")
        assertSuggests("before <t|")(Grammar.texTag)
        assertSelect(s"before <${Grammar.texTag}>|</${Grammar.texTag}>")
      }
    }
  }

}
