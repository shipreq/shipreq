package shipreq.webapp.base.test

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.test._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scala.scalajs.js
import shipreq.webapp.base.feature.AutoCompleteFeature.AutoComplete.Strategies
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.jsfacade.TextComplete
import shipreq.webapp.base.test.WebappTestUtil._
import sourcecode.Line
import teststate.domzipper.sizzle.Sizzle

object AutoCompleteTestUtil {

  final case class Backend($: BackendScope[Strategies, String]) extends AutoComplete.BackendTA {
    val domRef = Ref[html.TextArea]

    override def getTextFromHeadToCaret =
      AutoComplete.getTextFromHeadToCaretTA

    def getTextComplete(): TextComplete =
      textCompleteCBO.asCallback.runNow().get

    def render(state: String) = {
      def change = (e: ReactEventFromInput) => $.setState(e.target.value)
      <.textarea(
        ^.value := state,
        ^.onChange ==> change,
        ^.onBlur --> autoCompleteOnBlur,
        ^.onClick ==> autoCompleteOnClick,
        ^.onKeyDown ==> autoCompleteOnKeyDown,
      ).withRef(domRef)
    }

    val pxAutoComplete: Px[Strategies] =
      Px.props($).withoutReuse.autoRefresh

    override val autoCompleteCtx: CallbackOption[AutoCompleteCtx] =
      domRef.get.map(AutoCompleteCtx(pxAutoComplete.value(), _))
  }

  private val TestComponent = ScalaComponent.builder[Strategies]
    .initialState("")
    .renderBackend[Backend]
    .configure(AutoComplete.install)
    .build

  final case class AutoCompleteTestCtx(backend: Backend) {
    def setText(txt: String): Unit          = backend.$.setState(txt).runNow()
    def ta                  : html.TextArea = backend.domRef.get.asCallback.runNow().get
    def tc                  : TextComplete  = backend.getTextComplete()
    def txt                 : String        = ta.value

    // Sending events doesn't work.
    // Tried:
    //   SimEvent.Keyboard / Simulate
    //   jQuery.trigger
    //   dom.dispatchEvent
    // def pressKey(keyCode: Int): Unit =

    def pressDown(): Unit =
      tc.editor.emitMoveEvent(js.Dynamic.literal(code = "DOWN"))

    def pressEnter(): Unit =
      tc.editor.emitEnterEvent()

    def suggest(text: String): Unit = {
      setText(text.replace("|", ""))
      val n = ta
      var p = text.indexOf('|')
      if (p < 0) p = text.length
      n.setSelectionRange(p, p)
      n.selectionEnd = p
      val textBeforeCursor = n.value.take(p)

      // Hide prev dropdowns so they don't confuse expected results
      tc.hide()
      tc.editor.emitEscEvent()

      tc.trigger(textBeforeCursor)
    }
  }

  final case class SuggestionLabelSel(value: String)

  object SuggestionLabelSel {
    implicit val default = SuggestionLabelSel("")
  }

  def assertSuggests(input: String)(exp: String*)(implicit ctx: AutoCompleteTestCtx, ls: SuggestionLabelSel, l: Line): Unit = {
    ctx.suggest(input)
    assertEq(s"assertSuggests(${input.quote})", suggestions().map(_.label), exp.toVector)
  }

  private def allTextCompleteULs() =
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
    allTextCompleteULs()
      .filterNot(_.style.display == "none")
      .lastOption
      .map(ul => Sizzle(".textcomplete-item", ul).map(d => Suggestion(d.domAsHtml)).toVector)
      .getOrElse(Vector.empty)
  }

  def assertSelect(expectedTextAfterSelect: String)(implicit ctx: AutoCompleteTestCtx, ls: SuggestionLabelSel, l: Line): Unit = {
    ctx.pressDown()
    ctx.pressEnter()
    assertEq("Suggestions should disappear", suggestions().map(_.label), Vector.empty)
    val i = expectedTextAfterSelect.indexOf('|')
    if (i < 0)
      assertEq("assertSelect", ctx.txt.quote, expectedTextAfterSelect.quote)
    else {
      assertEq("assertSelect", ctx.txt.quote, expectedTextAfterSelect.replace("|", "").quote)
      assertCursorPos(i)
    }
  }

  def assertCursorPos(pos: Int)(implicit ctx: AutoCompleteTestCtx, l: Line): Unit = {
    val ta = ctx.ta
    assertEq((pos, pos), (ta.selectionStart, ta.selectionEnd))
  }

  def assertSuggestionsAndSelectionMultiline(input: String)
                                            (expectedSuggestions: String*)
                                            (expectedResult: String)
                                            (implicit ctx: AutoCompleteTestCtx, l: Line, beforeAfters: List[(String, String)]): Unit = {
    def go(before: String, after: String): Unit = {
      assertSuggests(before + input + "|" + after)(expectedSuggestions: _*)
      assertSelect(before + expectedResult + after)
    }
    for ((b, a) <- beforeAfters)
      go(b, a)
  }

  def withAutoComplete(strategies: Strategies)(t: AutoCompleteTestCtx => Unit): Unit =
    ReactTestUtils.withRenderedIntoBody(TestComponent(strategies)) { mounted =>
      val ctx = AutoCompleteTestCtx(mounted.backend)
      t(ctx)
    }

  def assertSuggestionsFor(input: String)
                          (expectedSuggestions: String*)
                          (implicit l: Line, s: Strategies, ls: SuggestionLabelSel): Unit =
    withAutoComplete(s) { implicit ctx =>
      assertSuggests(input)(expectedSuggestions: _*)
    }

  def assertSuggestionsAndSelectionFor(input: String)
                                      (expectedSuggestions: String*)
                                      (expectedResult: String)
                                      (implicit l: Line, s: Strategies, ls: SuggestionLabelSel): Unit =
    withAutoComplete(s) { implicit ctx =>
      assertSuggests(input)(expectedSuggestions: _*)
      assertSelect(expectedResult)
    }

}
