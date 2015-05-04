package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.nyaya.util.Multimap
import japgolly.scalajs.jquery.{TextComplete => TC}
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.test._
import org.scalajs.dom.ext.{KeyValue, KeyCode}
import scala.scalajs.js, js.Dynamic
import scalaz.std.string.stringInstance
import scalaz.std.vector.vectorEqual
import org.scalajs.dom.raw.HTMLInputElement
import utest._
import shipreq.base.util.MTrie.Ops
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.BaseTestUtil._
import shipreq.webapp.base.test.SampleProject2._
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.test.Sizzle

object AutoCompleteTest extends TestSuite {

  shipreq.webapp.client.app.ui.Style // Ensure initialised

  def editor(ac: TC.Strategies) =
    ReactComponentB[String]("AutoComplete test")
      .getInitialState(s => s)
      .render { $ =>
        def change = (e: ReactEventI) => $.setStateIO(e.target.value)
        <.input(^.`type` := "text", ^.value := $.state, ^.onChange ~~> change)
      }
      .componentDidMount { $ =>
        def n = $.getDOMNode().asInstanceOf[HTMLInputElement]
        UI.textComplete(n, ac, $.setStateIO(_))
      }
      .domType[HTMLInputElement]
      .build

  type Editor = ReactComponentM[_, String, _, TopNode]
  case class TestCtx(editor: Editor, acDomSel: String = "") {
    def $ = Dynamic.global.$(editor.getDOMNode())
  }

  def suggest(text: String)(implicit ctx: TestCtx): Unit = {
    ctx.editor.setState(text)
    ctx.$.textcomplete("trigger")
  }

  def suggestions(implicit ctx: TestCtx): Vector[String] =
    Sizzle(".textcomplete-item a " + ctx.acDomSel).map(_.textContent).toVector

  def test(input: String)(exp: String*)(implicit ctx: TestCtx): Unit = {
    suggest(input)
    assertEq(suggestions, exp.toVector)
  }

  def keydown(keyCode: Int)(implicit ctx: TestCtx): Unit = {
    val ev = Dynamic.global.$.Event("keydown", js.Dictionary("keyCode" -> keyCode))
    ctx.$.trigger(ev)
  }

  def testSelect(exp: String)(implicit ctx: TestCtx): Unit = {
    keydown(KeyCode.enter)
    assertEq(ctx.editor.state, exp)
  }

  // ===================================================================================================================

  lazy val acReqItems = AutoComplete.reqItems(project, plainText)
  lazy val acReqP     = AutoComplete.req(textSearch, acReqItems, prefix = true)
  lazy val cReqP      = ReactTestUtils renderIntoDocument editor(acReqP)("")

  // TODO Write more AutoComplete tests

  lazy val fakeTrie: ReqCode.Trie = {
    import shipreq.webapp.base.test.UnsafeTypes._

    val tgt: ReqCode.Data = ReqCode.ActiveData(1, 1)
    val codes = Set[ReqCode.Value](
      "aaaa1", "abc", "amp", "apple", "apply",
      "abc.around.1", "abc.around.2", "abc.around.tbc", "abc.around.torn", "abc.around.now",
      "abc.art", "abc.aqua", "abc.bark",
      "baa", "bcd", "c", "cant", "eggs", "1", "2a", "2b",
      "bcd.aaaz", "shit.eggs", "goat.damn.egg.stuff", "goat.damn.egg.crap", "goat.damn.egglike"
    )
    val t1 = codes.foldLeft(ReqCode.emptyTrie)((t, c) => t.put(c, tgt))

    val tomb = ReqCode.Data(None, Set(2), Multimap.empty)
    val tombCodes = Set[ReqCode.Value](
      "apple.dead", "ahhdead", "dead.eggs"
    )
    tombCodes.foldLeft(t1)((t, c) => t.put(c, tomb))
  }
  lazy val acReqCode  = AutoComplete.reqCode(fakeTrie)
  lazy val cReqCode   = ReactTestUtils renderIntoDocument editor(acReqCode)("")

  override def tests = TestSuite {

    'reqPrefixed {
      implicit val ctx = TestCtx(cReqP, "div div:first-child")

      def testMF(input: String)(exp: Int*): Unit =
        test(input)(exp.map("MF-" + _): _*)

      'pubid {
        testMF("[mf1")(1, 10, 11, 12, 13, 14, 15, 16, 17, 18)
        testMF("[mf8")(8)
        testMF("[mf-8")(8)
        test("[FR")("FR-1", "FR-2")
        testMF("[14")(14)
        testSelect("[MF-14] ")
      }
      'title {
        testMF("[level")(12, 22)
        testMF("[Collab")(9, 10, 11)
        testSelect("[MF-9] ")
      }
      'ignoreCase {
        testMF("[require")(12, 13, 22, 23, 24)
        testMF("[REQUIRE")(12, 13, 22, 23, 24)
      }
    }

    'reqCode {
      implicit val ctx = TestCtx(cReqCode)
      'root {
        test("a")("aaaa1", "abc", "amp", "apple", "apply")
        testSelect("aaaa1")
        test("ap")("apple", "apply")
        test("b")("baa", "bcd")
        test("2")("2a", "2b")
        test("d")() // no dead.eggs
      }
      'soleExact {
        test("1")()
        test("c")("c", "cant")
      }
      'path {
        test("abc.a")("aqua", "around", "art")
        test("abc.ar")("around", "art")
        test("abc.arou")("around")
        test("abc.around.t")("tbc", "torn")
        testSelect("abc.around.tbc")
      }
      'open {
        test("abc.")("aqua", "around", "art", "bark")
        test("abc.around.")("1", "2", "now", "tbc", "torn")
        test("apply.")()
        test("bcd.")("aaaz")
        testSelect("bcd.aaaz")
      }
      'dot - {
        test("x"*100)() // Clear previous
        test(".")()
      }
      'mid {
        test(".eg")("goat.damn.egg", "goat.damn.egglike", "shit.eggs")
        test(".egg")("goat.damn.egg", "goat.damn.egglike", "shit.eggs")
        test(".eggs")("shit.eggs")
      }
    }

  }
}
