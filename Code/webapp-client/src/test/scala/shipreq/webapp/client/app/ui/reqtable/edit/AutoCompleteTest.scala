package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.nyaya.util.Multimap
import japgolly.scalajs.jquery.{TextComplete => TC}
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.test._
import org.scalajs.dom.ext.{KeyValue, KeyCode}
import scala.scalajs.js, js.Dynamic
import scalaz.effect.IO
import scalaz.std.anyVal._
import scalaz.std.string.stringInstance
import scalaz.std.tuple.tuple2Equal
import scalaz.std.vector.vectorEqual
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLTextAreaElement
import utest._

import shipreq.base.util.MTrie.Ops
import shipreq.webapp.base.data._
import shipreq.webapp.base.test._, BaseTestUtil._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.test.{PrepareEnv, Sizzle}
import shipreq.webapp.client.util.{Plain, Contextualise}
import SampleProject3._

object AutoCompleteTest extends TestSuite {
  PrepareEnv()

  type N = HTMLTextAreaElement

  // Not using React because of https://github.com/ariya/phantomjs/issues/12493
  // type Editor = ReactComponentM[_, String, _, N]

  class Editor(n: N) {
    def state: String                   = n.value
    def setState(s: String): Unit       = n.value = s
    def setStateIO(s: String): IO[Unit] = IO(setState(s))
    def getDOMNode(): N                 = n
  }

  def editor(ac: TC.Strategies) = {
    //    ReactComponentB[String]("AutoComplete test")
    //      .getInitialState(s => s)
    //      .render { $ =>
    //        def change = (e: ReactEventI) => $.setStateIO(e.target.value)
    //        <.textarea(^.value := $.state, ^.onChange ~~> change)
    //      }
    //      .domType[N]
    //      .componentDidMount { $ =>
    //        def n = $.getDOMNode()
    //        UI.textComplete(n, ac, $.setStateIO(_))
    //        document.body.appendChild(n)
    //      }
    //      .build
    //    ReactTestUtils renderIntoDocument editor(c)("")

    val n = document.createElement("textarea").asInstanceOf[HTMLTextAreaElement]
    document.body.appendChild(n) // https://github.com/ariya/phantomjs/issues/12493
    val e = new Editor(n)
    UI.textComplete(n, ac, e.setStateIO)
    e
  }

  case class TestCtx(editor: Editor, acDomSel: String = "") {
    def $ = Dynamic.global.$(editor.getDOMNode())
  }

  def suggest(text: String)(implicit ctx: TestCtx): Unit = {
    ctx.editor.setState(text.replace("|", ""))
    val n = ctx.editor.getDOMNode()
    var p = text.indexOf('|')
    if (p < 0) p = text.length
    n.setSelectionRange(p, p)
    n.selectionEnd = p
    ctx.$.textcomplete("trigger")
  }

  def suggestions(implicit ctx: TestCtx): Vector[String] = {
    // println(Sizzle(".textcomplete-item").headOption.fold("NADA!")(_.outerHTML))
    Sizzle(".textcomplete-item a " + ctx.acDomSel).map(_.textContent).toVector
  }

  def quote(s: String) = s""""${s.replace("\n", "\\n")}""""

  def test(input: String)(exp: String*)(implicit ctx: TestCtx): Unit = {
    suggest(input)
    assertEq(quote(input), suggestions, exp.toVector)
  }

  def testML(input: String)(exp: String*)(select: String)(implicit ctx: TestCtx, beforeAfters: List[(String, String)]): Unit = {
    def go(before: String, after: String): Unit = {
      test(before + input + "|" + after)(exp: _*)
      testSelect(before + select + after)
    }
    for ((b, a) <- beforeAfters) go(b, a)
  }

  def keydown(keyCode: Int)(implicit ctx: TestCtx): Unit = {
    val ev = Dynamic.global.$.Event("keydown", js.Dictionary("keyCode" -> keyCode))
    ctx.$.trigger(ev)
  }

  def testSelect(exp: String)(implicit ctx: TestCtx): Unit = {
    keydown(KeyCode.enter)
    val i = exp.indexOf('|')
    if (i < 0)
      assertEq(quote(ctx.editor.state), quote(exp))
    else {
      assertEq(quote(ctx.editor.state), quote(exp.replace("|", "")))
      testCursorPos(i)
    }
  }

  def testCursorPos(pos: Int)(implicit ctx: TestCtx): Unit = {
    val n = ctx.editor.getDOMNode()
    assertEq((pos, pos), (n.selectionStart, n.selectionEnd))
  }

  // ===================================================================================================================

  lazy val acReqItems = AutoComplete.reqItems(project, plainText)
  lazy val acReqC     = AutoComplete.req(textSearch, acReqItems, Contextualise)
  lazy val cReqC      = editor(acReqC)

  lazy val cReqCodePrefixes = editor(AutoComplete.reqCode.prefixes(fakeTrie))
  lazy val cReqCodeRefs     = editor(AutoComplete.reqCode.ref(project2, plainText2))

  lazy val cIssuesC = editor(AutoComplete.issue(project.customIssueTypes.data.values.toStream)(Contextualise))

  lazy val cTagsC = editor(AutoComplete.tag(project.atags)(Contextualise))
  lazy val cTagsP = editor(AutoComplete.tag(project.atags)(Plain))

  // ReqCode data - uses SampleProject2

  lazy val fakeTrie: ReqCode.Trie = {
    import shipreq.webapp.base.test.UnsafeTypes._

    val codes = Set[ReqCode.Value](
      "aaaa1", "abc", "amp", "apple", "apply",
      "abc.around.1", "abc.around.2", "abc.around.tbc", "abc.around.torn", "abc.around.now",
      "abc.art", "abc.aqua", "abc.bark",
      "baa", "bcd", "c", "cant", "eggs", "1", "2a", "2b",
      "bcd.aaaz", "shit.eggs", "goat.damn.egg.stuff", "goat.damn.egg.crap", "goat.damn.egglike"
    )

    val nextId: () => ReqCodeId = {
      var v = 0
      () => { v += 1; v}
    }
    def tgt: ReqCode.Data = ReqCode.ActiveData(nextId(), 1)
    val t1 = codes.foldLeft(ReqCode.emptyTrie)((t, c) => t.put(c, tgt))

    def tomb = ReqCode.Data(None, Set(nextId()), Multimap.empty)
    val tombCodes = Set[ReqCode.Value](
      "apple.dead", "ahhdead", "dead.eggs"
    )
    tombCodes.foldLeft(t1)((t, c) => t.put(c, tomb))
  }
  lazy val project2 = {
    import ProjectDSL._, UnsafeTypes._
    val p = (Project.reqCodes ^|-> RevAnd.data).set(ReqCodes(fakeTrie))(SampleProject2.project)
    (DeadReqCode("dead.ref", target = 1, id = 90) + DeadReqCode("dead.group", id = 91)) ! p
  }
  lazy val plainText2 = PlainText(project2)

  override def tests = TestSuite {

    'issue {
      implicit val ctx = TestCtx(cIssuesC)
      test("#xxxxxxx")() // Clear previous

      'start {
        test("#T")("TBD", "TODO")
        testSelect("#TBD ")
      }
      'mid {
        test("#DO")("TODO")
        testSelect("#TODO ")
      }
      'noSyntax -
        test("T")()
      'filterDead -
        test("#D")("TBD", "TODO") // PENDING is dead
    }

    'tagC {
      implicit val ctx = TestCtx(cTagsC)
      test("#xxxxxxx")() // Clear previous

      'start {
        test("#pri")("pri=high", "pri=low", "pri=med")
        testSelect("#pri=high ")
      }
      'mid {
        test("#1")("v1.0", "v1.1", "v1.2", "v1.x")
        testSelect("#v1.0 ")
      }
      'noSyntax -
        test("pri")()
      'filterDead -
        test("#x")("v1.x", "v2.x") // v3.x is dead
    }

    'tagP {
      implicit val ctx = TestCtx(cTagsP)
      test("xxxxxxx")() // Clear previous

      'start {
        test("pri")("pri=high", "pri=low", "pri=med")
        testSelect("pri=high ")
      }
      'mid {
        test("1")("v1.0", "v1.1", "v1.2", "v1.x")
        testSelect("v1.0 ")
      }
      'withSyntax - {
        test("#pri")("pri=high", "pri=low", "pri=med")
        testSelect("pri=high ")
      }
      'filterDead -
        test("#x")("v1.x", "v2.x") // v3.x is dead
    }

    'reqC {
      implicit val ctx = TestCtx(cReqC, "div div:first-child")

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
        testMF("[save")(17)
        testMF("[Collab")(9, 10, 11)
        testSelect("[MF-9] ")
      }
      'ignoreCase {
        testMF("[require")(12, 13, 22, 23, 24)
        testMF("[REQUIRE")(12, 13, 22, 23, 24)
      }
      'filterDeadByPubid -
        testMF("[mf28")()
      'filterDeadByTitle -
        testMF("[search")(25) // excludes CO-{1,2}
    }

    'reqCodePrefixes {
      implicit val ctx = TestCtx(cReqCodePrefixes)
      implicit val multilineData = List[(String, String)](
        ("hehe.no\n", ""),
        ("", "\nhehe.no"),
        ("omg.yay\nhehe.no\n", ""),
        ("", "\nomg.yay\nhehe.no"),
        ("omg.yay\n", "\nhehe.no"),
        ("more.again\nomg.yay\n", "\nhehe.no\nok.then"))

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
      'multilinePrefix -
        testML("ap")("apple", "apply")("apple")
      'multilineMid -
        testML(".eggs")("shit.eggs")("shit.eggs")
      'filterDead -
        test("dea")()
    }

    'reqCodeRefs {
      implicit val ctx = TestCtx(cReqCodeRefs, "div > div > span:first-child")
      'root {
        test("[app")("apple", "apply")
        testSelect("[apple] ")
        test("[goa")("goat.damn.egg.crap", "goat.damn.egg.stuff", "goat.damn.egglike")
      }
      'mid -
        test("[aqu")("abc.aqua")
      'soleExact -
        test("[aaaa1")("aaaa1")
      'path {
        val arounds = List("abc.around.1", "abc.around.2", "abc.around.now", "abc.around.tbc", "abc.around.torn")
        test("[abc.arou")(arounds: _*)
        testSelect("[abc.around.1] ")
        test("[abc.round")(arounds: _*)
        test("[a.round")(arounds: _*)
        test("[c.round")(arounds: _*)
        test("[g.d.e.s")("goat.damn.egg.stuff")
      }
      'skipNode {
        test("[g.e.s")("goat.damn.egg.stuff")
        test("[a.now")("abc.around.now")
        test("[abc.t")("abc.around.tbc", "abc.around.torn", "abc.art")
        test("[arou.t")("abc.around.tbc", "abc.around.torn")
      }
      'dotStart -
        test("[.egg")("goat.damn.egg.crap", "goat.damn.egg.stuff", "goat.damn.egglike", "shit.eggs")
      'dotEnd -
        test("[egg.")("goat.damn.egg.crap", "goat.damn.egg.stuff")
      'filterDead -
        test("[dea")()
    }

    'math {
      implicit val ctx = TestCtx(editor(AutoComplete.math))
      test("<m")("math")
      testSelect("<math>|</math>")
      test("before <m|")("math")
      testSelect("before <math>|</math>")
    }
  }
}
