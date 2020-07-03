package shipreq.webapp.base.feature.autocomplete.strategies

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.test._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scala.scalajs.js
import shipreq.base.util.MTrie.Ops
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AutoCompleteFeature.AutoComplete.Strategies
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.jsfacade.TextComplete
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.{PlainText, TextSearch}
import sourcecode.Line
import teststate.domzipper.sizzle.Sizzle

object AutoCompleteTestUtil {

  final case class Backend($: BackendScope[Strategies, String]) extends AutoComplete.BackendTA {
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

    val pxAutoComplete: Px[Strategies] =
      Px.props($).withoutReuse.autoRefresh

    override val autoCompleteCtx: CallbackOption[AutoCompleteCtx] =
      domRef.get.map(AutoCompleteCtx(pxAutoComplete.value(), _))
  }

  val TestComponent = ScalaComponent.builder[Strategies]
    .initialState("")
    .renderBackend[Backend]
    .configure(AutoComplete.install)
    .build

  final case class TestCtx(backend: Backend) {
    def setText(txt: String): Unit          = backend.$.setState(txt).runNow()
    def ta                  : html.TextArea = backend.domRef.get.asCallback.runNow().get
    def tc                  : TextComplete  = backend.getTextComplete()
    def txt                 : String        = ta.value
  }

  final case class SuggestionLabelSel(value: String)

  object SuggestionLabelSel {
    implicit val default = SuggestionLabelSel("")
  }

  def quote(s: String) = s""""${s.replace("\n", "\\n")}""""

  def assertSuggests(input: String)(exp: String*)(implicit ctx: TestCtx, ls: SuggestionLabelSel, l: Line): Unit = {
    suggest(input)
    assertEq(s"assertSuggests(${quote(input)})", suggestions().map(_.label), exp.toVector)
  }

  def suggest(text: String)(implicit ctx: TestCtx): Unit = {
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

  def allTextCompleteULs =
    Sizzle("ul.textcomplete-dropdown").map(_.asInstanceOf[html.UList])

  final case class Suggestion(dom: html.Element)(implicit labelSel: SuggestionLabelSel) {
    def label: String =
        labelSel.value match {
          case "" => dom.textContent
          case s  =>
            Option(dom.querySelector("a " + s))
              .orElse(Option(dom.querySelector("a")))
              .getOrElse(dom)
              .textContent
        }
  }

  def suggestions()(implicit ls: SuggestionLabelSel): Vector[Suggestion] = {
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
  // def pressKey(keyCode: Int)(implicit ctx: TestCtx): Unit =

  def pressDown()(implicit ctx: TestCtx): Unit =
    ctx.tc.editor.emitMoveEvent(js.Dynamic.literal(code = "DOWN"))

  def pressEnter()(implicit ctx: TestCtx): Unit =
    ctx.tc.editor.emitEnterEvent()

  def assertSelect(expectedTextAfterSelect: String)(implicit ctx: TestCtx, ls: SuggestionLabelSel, l: Line): Unit = {
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

  def assertCursorPos(pos: Int)(implicit ctx: TestCtx, l: Line): Unit = {
    val ta = ctx.ta
    assertEq((pos, pos), (ta.selectionStart, ta.selectionEnd))
  }

  def testMultiline(input: String)
                   (expectedSuggestions: String*)
                   (expectedResult: String)
                   (implicit ctx: TestCtx, l: Line, beforeAfters: List[(String, String)]): Unit = {
    def go(before: String, after: String): Unit = {
      assertSuggests(before + input + "|" + after)(expectedSuggestions: _*)
      assertSelect(before + expectedResult + after)
    }
    for ((b, a) <- beforeAfters) go(b, a)
  }

  def test(strategies: Strategies)(t: TestCtx => Unit): Unit =
    ReactTestUtils.withRenderedIntoBody(TestComponent(strategies)) { mounted =>
      val ctx = TestCtx(mounted.backend)
      t(ctx)
    }

  def quickTestSuggestions(input: String)
                          (expectedSuggestions: String*)
                          (implicit l: Line, s: Strategies, ls: SuggestionLabelSel): Unit =
    test(s) { implicit ctx =>
      assertSuggests(input)(expectedSuggestions: _*)
    }

  def quickTestSuggestionsAndSelection(input: String)
                                      (expectedSuggestions: String*)
                                      (expectedResult: String)
                                      (implicit l: Line, s: Strategies, ls: SuggestionLabelSel): Unit =
    test(s) { implicit ctx =>
      assertSuggests(input)(expectedSuggestions: _*)
      assertSelect(expectedResult)
    }

  // ===================================================================================================================

  lazy val fakeTrie: ReqCode.Trie = {
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

  lazy val project2 = {
    import ProjectDsl._
    import UnsafeTypes._
    val p = Project.reqCodes.set(ReqCodes(fakeTrie))(SampleProject2.project)
    (DeadReqCode("dead.ref", oldReqId = 1, id = Some(ApReqCodeId(90))) +
      DeadReqCode("dead.group", id = Some(ReqCodeGroupId(91)))) ! p
  }

  lazy val plainText2 = PlainText.ForProject.noCtx(project2)
  lazy val textSearch2 = TextSearch(project2, plainText2)

}
