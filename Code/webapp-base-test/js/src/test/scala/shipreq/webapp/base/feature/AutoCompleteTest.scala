package shipreq.webapp.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.test._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scala.scalajs.js
import shipreq.base.util.MTrie.Ops
import shipreq.webapp.base.data.{Contextualise, Plain, _}
import shipreq.webapp.base.feature.AutoCompleteFeature.AutoComplete.Strategies
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.jsfacade.TextComplete
import shipreq.webapp.base.test.SampleProject3._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.{Grammar, PlainText}
import sourcecode.Line
import teststate.domzipper.sizzle.Sizzle
import utest._

object AutoCompleteTest extends TestSuite {

  private case class Backend($: BackendScope[AutoComplete.Strategies, String]) extends AutoComplete.BackendTA {
    val domRef = Ref[html.TextArea]

    def getTextComplete(): TextComplete =
      textCompleteCBO.asCallback.runNow().get

    def render(state: String) = {
      def change = (e: ReactEventFromInput) => $.setState(e.target.value)
      <.textarea(
        ^.value := state,
        ^.onChange ==> change,
        ^.onBlur --> autoCompleteOnBlur,
        ^.onClick ==> autoCompleteOnClick,
      ).withRef(domRef)
    }

    private val pxAutoComplete: Px[AutoComplete.Strategies] =
      Px.props($).withoutReuse.autoRefresh

    override val autoCompleteCtx: CallbackOption[AutoCompleteCtx] =
      domRef.get.map(AutoCompleteCtx(pxAutoComplete.value(), _))
  }

  private val TestComponent = ScalaComponent.builder[AutoComplete.Strategies]
    .initialState("")
    .renderBackend[Backend]
    .configure(AutoComplete.install)
    .build

  private case class TestCtx(backend: Backend) {
    def setText(txt: String): Unit          = backend.$.setState(txt).runNow()
    def ta                  : html.TextArea = backend.domRef.get.asCallback.runNow().get
    def tc                  : TextComplete  = backend.getTextComplete()
    def txt                 : String        = ta.value
  }

  private case class SuggestionLabelSel(value: String)

  private object SuggestionLabelSel {
    implicit val default = SuggestionLabelSel("")
  }

  private def quote(s: String) = s""""${s.replace("\n", "\\n")}""""

  private def assertSuggests(input: String)(exp: String*)(implicit ctx: TestCtx, ls: SuggestionLabelSel, l: Line): Unit = {
    suggest(input)
    assertEq(s"assertSuggests(${quote(input)})", suggestions().map(_.label), exp.toVector)
  }

  private def suggest(text: String)(implicit ctx: TestCtx): Unit = {
    ctx.setText(text.replace("|", ""))
    val n = ctx.ta
    var p = text.indexOf('|')
    if (p < 0) p = text.length
    n.setSelectionRange(p, p)
    n.selectionEnd = p
    val textBeforeCursor = n.value.take(p)

    // Hide prev dropdowns so they don't confuse expected results
    ctx.tc.hide()
    ctx.tc.editor.emitEscEvent()

    ctx.tc.trigger(textBeforeCursor)
  }

  private def allTextCompleteULs =
    Sizzle("ul.textcomplete-dropdown").map(_.asInstanceOf[html.UList])

  private final case class Suggestion(dom: html.Element)(implicit labelSel: SuggestionLabelSel) {
    def label = labelSel.value match {
      case "" => dom.textContent
      case s  => dom.querySelector("a " + s).textContent
    }
  }

  private def suggestions()(implicit ls: SuggestionLabelSel): Vector[Suggestion] = {
    // println(org.scalajs.dom.document.body.innerHTML)
    allTextCompleteULs
      .filterNot(_.style.display == "none")
      .lastOption
      .map(ul => Sizzle(".textcomplete-item", ul).map(d => Suggestion(d.domAsHtml)).toVector)
      .getOrElse(Vector.empty)
  }

  // Sending events doesn't work.
  // Tried:
  //   SimEvent.Keyboard / Simulate
  //   jQuery.trigger
  //   dom.dispatchEvent
  // private def pressKey(keyCode: Int)(implicit ctx: TestCtx): Unit =

  private def pressDown()(implicit ctx: TestCtx): Unit =
    ctx.tc.editor.emitMoveEvent(js.Dynamic.literal(code = "DOWN"))

  private def pressEnter()(implicit ctx: TestCtx): Unit =
    ctx.tc.editor.emitEnterEvent()

  private def assertSelect(expectedTextAfterSelect: String)(implicit ctx: TestCtx, ls: SuggestionLabelSel, l: Line): Unit = {
    pressDown()
    pressEnter()
    assertEq("Suggestions should disappear", suggestions().map(_.label), Vector.empty)
    val i = expectedTextAfterSelect.indexOf('|')
    if (i < 0)
      assertEq("assertSelect", quote(ctx.txt), quote(expectedTextAfterSelect))
    else {
      assertEq("assertSelect", quote(ctx.txt), quote(expectedTextAfterSelect.replace("|", "")))
      assertCursorPos(i)
    }
  }

  private def assertCursorPos(pos: Int)(implicit ctx: TestCtx, l: Line): Unit = {
    val ta = ctx.ta
    assertEq((pos, pos), (ta.selectionStart, ta.selectionEnd))
  }

  private def testMultiline(input: String)
                           (expectedSuggestions: String*)
                           (expectedResult: String)
                           (implicit ctx: TestCtx, l: Line, beforeAfters: List[(String, String)]): Unit = {
    def go(before: String, after: String): Unit = {
      assertSuggests(before + input + "|" + after)(expectedSuggestions: _*)
      assertSelect(before + expectedResult + after)
    }
    for ((b, a) <- beforeAfters) go(b, a)
  }

  private def test(strategies: AutoComplete.Strategies)(t: TestCtx => Unit): Unit =
    ReactTestUtils.withRenderedIntoBody(TestComponent(strategies)) { mounted =>
      val ctx = TestCtx(mounted.backend)
      t(ctx)
    }

  private def quickTestSuggestions(input: String)
                                  (expectedSuggestions: String*)
                                  (implicit l: Line, s: Strategies, ls: SuggestionLabelSel): Unit =
    test(s) { implicit ctx =>
      assertSuggests(input)(expectedSuggestions: _*)
    }

  private def quickTestSuggestionsAndSelection(input: String)
                                              (expectedSuggestions: String*)
                                              (expectedResult: String)
                                              (implicit l: Line, s: Strategies, ls: SuggestionLabelSel): Unit =
    test(s) { implicit ctx =>
      assertSuggests(input)(expectedSuggestions: _*)
      assertSelect(expectedResult)
    }

  // ===================================================================================================================

  private lazy val fakeTrie: ReqCode.Trie = {
    import shipreq.webapp.base.test.UnsafeTypes._

    val codes = Set[ReqCode.Value](
      "aaaa1", "abc", "amp", "apple", "apply",
      "abc.around.1", "abc.around.2", "abc.around.tbc", "abc.around.torn", "abc.around.now",
      "abc.art", "abc.aqua", "abc.bark",
      "baa", "bcd", "c", "cant", "eggs", "1", "2a", "2b",
      "bcd.aaaz", "shit.eggs", "goat.damn.egg.stuff", "goat.damn.egg.crap", "goat.damn.egglike"
    )

    val nextApReqCodeId: () => ApReqCodeId = {
      var v = 0
      () => { v += 1; ApReqCodeId(v)}
    }
    def nextReqCodeGroupId(): ReqCodeGroupId = {
      val id = nextApReqCodeId().value
      ReqCodeGroupId(id)
    }
    def tgt: ReqCode.Data = ReqCode.ActiveReq(nextApReqCodeId(), 1, None, ReqCode.emptyReqInactive)
    val t1 = codes.foldLeft(ReqCode.Trie.empty)((t, c) => t.put(c, tgt))

    def tomb = ReqCode.Data.empty.copy(deadGroup = Some(DeadCodeGroup(nextReqCodeGroupId(), "asdf")))
    val tombCodes = Set[ReqCode.Value](
      "apple.dead", "ahhdead", "dead.eggs"
    )
    tombCodes.foldLeft(t1)((t, c) => t.put(c, tomb))
  }

  private lazy val project2 = {
    import ProjectDsl._
    import UnsafeTypes._
    val p = Project.reqCodes.set(ReqCodes(fakeTrie))(SampleProject2.project)
    (DeadReqCode("dead.ref", oldReqId = 1, id = Some(ApReqCodeId(90))) +
      DeadReqCode("dead.group", id = Some(ReqCodeGroupId(91)))) ! p
  }

  private lazy val plainText2 = PlainText.ForProject.noCtx(project2)

  override def tests = Tests {

    "issue" - {
      implicit val strategies =
        AutoComplete.Project.hashtag(
          issues = project.config.customIssueTypes.values,
          tags = Nil,
          fd = HideDead)(Contextualise)

      "start"      - quickTestSuggestionsAndSelection("#T")("TBD", "TODO")("#TBD")
      "mid"        - quickTestSuggestionsAndSelection("#DO")("TODO")("#TODO")
      "noSyntax"   - quickTestSuggestions("T")()
      "filterDead" - quickTestSuggestions("#D")("TBD", "TODO") // PENDING is dead
      "heading"    - quickTestSuggestions("##")()
    }

    "tagC" - {
      implicit val strategies =
        AutoComplete.Project.tag(project.config.tags.applicableTagIterator().toList, HideDead)(Contextualise)

      "start"      - quickTestSuggestionsAndSelection("#pri")("pri=high", "pri=low", "pri=med")("#pri=high")
      "mid"        - quickTestSuggestionsAndSelection("#1")("v1.0", "v1.1", "v1.2", "v1.3", "v1.x")("#v1.0")
      "noSyntax"   - quickTestSuggestions("pri")()
      "filterDead" - quickTestSuggestions("#x")("v1.x", "v2.x") // v3.x & v4.x are dead
    }

    "tagP" - {
      implicit val strategies =
        AutoComplete.Project.tag(project.config.tags.applicableTagIterator().toList, HideDead)(Plain)

      "start"      - quickTestSuggestionsAndSelection("pri")("pri=high", "pri=low", "pri=med")("pri=high")
      "mid"        - quickTestSuggestionsAndSelection("1")("v1.0", "v1.1", "v1.2", "v1.3", "v1.x")("v1.0")
      "withSyntax" - quickTestSuggestionsAndSelection("#pri")("pri=high", "pri=low", "pri=med")("pri=high")
      "filterDead" - quickTestSuggestions("#x")("v1.x", "v2.x") // v3.x & v4.x are dead
    }

    "reqC" - {
      implicit val strategies =
        AutoComplete.Project.req(
          textSearch,
          AutoComplete.Project.reqItems(project, plainText),
          Contextualise)

      implicit val labelSel = SuggestionLabelSel("div div:first-child")

      def assertSuggestsMFs(input: String)(exp: Int*)(implicit ctx: TestCtx): Unit =
        assertSuggests(input)(exp.map("MF-" + _): _*)

      "pubid" - test(strategies) { implicit ctx =>
        assertSuggestsMFs("[mf1")(1, 10, 11, 12, 13, 14, 15, 16, 17, 18)
        assertSuggestsMFs("[mf8")(8)
        assertSuggestsMFs("[mf-8")(8)
        assertSuggests("[FR")("FR-1", "FR-2")
        assertSuggestsMFs("[14")(14)
        assertSelect("[MF-14] ")
      }
      "title" - test(strategies) { implicit ctx =>
        assertSuggestsMFs("[save")(17)
        assertSuggestsMFs("[Collab")(9, 10, 11)
        assertSelect("[MF-9] ")
      }
      "ignoreCase" - test(strategies) { implicit ctx =>
        assertSuggestsMFs("[require")(12, 13, 22, 23, 24)
        assertSuggestsMFs("[REQUIRE")(12, 13, 22, 23, 24)
      }
      "filterDeadByPubid" - quickTestSuggestions("[mf28")()
      "filterDeadByTitle" - quickTestSuggestions("[search")("MF-25") // excludes CO-{1,2}
    }

    "reqCodePrefixes" - {
      implicit val strategies =
        AutoComplete.Project.reqCode.prefixes(fakeTrie)

      implicit val multilineData = List[(String, String)](
        ("hehe.no\n", ""),
        ("", "\nhehe.no"),
        ("omg.yay\nhehe.no\n", ""),
        ("", "\nomg.yay\nhehe.no"),
        ("omg.yay\n", "\nhehe.no"),
        ("more.again\nomg.yay\n", "\nhehe.no\nok.then"))

      "root" - test(strategies) { implicit ctx =>
        assertSuggests("a")("aaaa1", "abc", "amp", "apple", "apply")
        assertSelect("aaaa1")
        assertSuggests("ap")("apple", "apply")
        assertSuggests("b")("baa", "bcd")
        assertSuggests("2")("2a", "2b")
        assertSuggests("d")() // no dead.eggs
      }
      "soleExact" - test(strategies) { implicit ctx =>
        assertSuggests("1")()
        assertSuggests("c")("c", "cant")
      }
      "path" - test(strategies) { implicit ctx =>
        assertSuggests("abc.a")("aqua", "around", "art")
        assertSuggests("abc.ar")("around", "art")
        assertSuggests("abc.arou")("around")
        assertSuggests("abc.around.t")("tbc", "torn")
        assertSelect("abc.around.tbc")
      }
      "open" - test(strategies) { implicit ctx =>
        assertSuggests("abc.")("aqua", "around", "art", "bark")
        assertSuggests("abc.around.")("1", "2", "now", "tbc", "torn")
        assertSuggests("apply.")()
        assertSuggests("bcd.")("aaaz")
        assertSelect("bcd.aaaz")
      }
      "dot" - test(strategies) { implicit ctx =>
        assertSuggests("x"*100)() // Clear previous
        assertSuggests(".")()
      }
      "mid" - test(strategies) { implicit ctx =>
        assertSuggests(".eg")("goat.damn.egg", "goat.damn.egglike", "shit.eggs")
        assertSuggests(".egg")("goat.damn.egg", "goat.damn.egglike", "shit.eggs")
        assertSuggests(".eggs")("shit.eggs")
      }
      "multilinePrefix" - test(strategies) { implicit ctx => testMultiline("ap")("apple", "apply")("apple") }
      "multilineMid"    - test(strategies) { implicit ctx => testMultiline(".eggs")("shit.eggs")("shit.eggs") }
      "filterDead"      - quickTestSuggestions("dea")()
    }

    "reqCodeRefs" - {
      implicit val strategies =
        AutoComplete.Project.reqCode.ref(project2, plainText2)

      implicit val labelSel =
        SuggestionLabelSel("div > div > span:first-child")

      "root" - test(strategies) { implicit ctx =>
        assertSuggests("[app")("apple", "apply")
        assertSelect("[apple] ")
        assertSuggests("[goa")("goat.damn.egg.crap", "goat.damn.egg.stuff", "goat.damn.egglike")
      }
      "path" - test(strategies) { implicit ctx =>
        val arounds = List("abc.around.1", "abc.around.2", "abc.around.now", "abc.around.tbc", "abc.around.torn")
        assertSuggests("[abc.arou")(arounds: _*)
        assertSelect("[abc.around.1] ")
        assertSuggests("[abc.round")(arounds: _*)
        assertSuggests("[a.round")(arounds: _*)
        assertSuggests("[c.round")(arounds: _*)
        assertSuggests("[g.d.e.s")("goat.damn.egg.stuff")
      }
      "skipNode" - test(strategies) { implicit ctx =>
        assertSuggests("[g.e.s")("goat.damn.egg.stuff")
        assertSuggests("[a.now")("abc.around.now")
        assertSuggests("[abc.t")("abc.around.tbc", "abc.around.torn", "abc.art")
        assertSuggests("[arou.t")("abc.around.tbc", "abc.around.torn")
      }
      "mid"        - quickTestSuggestions("[aqu")("abc.aqua")
      "soleExact"  - quickTestSuggestions("[aaaa1")("aaaa1")
      "dotStart"   - quickTestSuggestions("[.egg")("goat.damn.egg.crap", "goat.damn.egg.stuff", "goat.damn.egglike", "shit.eggs")
      "dotEnd"     - quickTestSuggestions("[egg.")("goat.damn.egg.crap", "goat.damn.egg.stuff")
      "filterDead" - quickTestSuggestions("[dea")()
    }

    "tex" - test(AutoComplete.Project.tex) { implicit ctx =>
      assertSuggests("<t")(Grammar.texTag)
      assertSelect(s"<${Grammar.texTag}>|</${Grammar.texTag}>")
      assertSuggests("before <t|")(Grammar.texTag)
      assertSelect(s"before <${Grammar.texTag}>|</${Grammar.texTag}>")
    }
  }
}
