package shipreq.webapp.base.feature

/*
// TODO Re-enable AutoComplete tests
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.test._
import org.scalajs.dom.ext.KeyCode
import scala.scalajs.js, js.Dynamic
import org.scalajs.dom.document
import org.scalajs.dom.raw.{HTMLTextAreaElement, HTMLUListElement}
import teststate.domzipper.sizzle.Sizzle
import utest._
import shipreq.base.util.MTrie.Ops
import shipreq.webapp.base.data._
import shipreq.webapp.base.test._, WebappTestUtil._
import shipreq.webapp.base.text.{PlainText, ProjectText}
import shipreq.webapp.base.data.{Plain, Contextualise}
import AutoCompleteFeature._
import SampleProject3._

object AutoCompleteTest extends TestSuite {

  type N = HTMLTextAreaElement

  // Not using React because of https://github.com/ariya/phantomjs/issues/12493
  // type Editor = ReactComponentM[_, String, _, N]

  class Backend($: BackendScope[AutoComplete.Strategies, String]) extends AutoComplete.BackendTA {
    def render(state: String) = {
      def change = (e: ReactEventFromInput) => $.setState(e.target.value)
      <.textarea(^.value := state, ^.onChange ==> change)
    }

    override val autoCompleteCtx: CallbackTo[AutoCompleteCtx] =
      for {
        p <- $.props
        n <- $.getDOMNode.map(_.asElement)
      } yield AutoCompleteCtx(p, n.domCast[N])
  }
  val Tester = ScalaComponent.builder[AutoComplete.Strategies]("AutoComplete test")
    .initialState("")
    .renderBackend[Backend]
    .build

  trait Editor {
    def setState(s: String): Unit
    def getDOMNode: N
    def state: String = getDOMNode.map(_.asElement).value
  }

  def editor(ac: AutoComplete.Strategies): Editor = {
    val m = ReactTestUtils renderIntoBody Tester(ac)
    new Editor {
      override def setState(s: String) = m.setState(s)
      override def getDOMNode: N       = m.getDOMNode.domCast[N]
    }

//    val n = document.createElement("textarea").asInstanceOf[HTMLTextAreaElement]
//    document.body.appendChild(n) // https://github.com/ariya/phantomjs/issues/12493
//    val e = new Editor(n)
//    val c = AutoComplete.Ctx(ac, n)
//    autocomplete.ForComponent.lowLevelInstall(c).runNow()
//    e
  }

  case class TestCtx(editor: Editor, acDomSel: String = "") {
    def $ = Dynamic.global.$(editor.getDOMNode.map(_.asElement))
  }

  def allTextCompleteULs =
    Sizzle("ul.textcomplete-dropdown").map(_.asInstanceOf[HTMLUListElement])

  def suggest(text: String)(implicit ctx: TestCtx): Unit = {
    // Hide prev dropdowns so they don't confuse expected results
    val uls = allTextCompleteULs
    if (uls.length > 1)
      uls.dropRight(1).foreach(_.style.display = "none")

    ctx.editor.setState(text.replace("|", ""))
    val n = ctx.editor.getDOMNode.map(_.asElement)
    var p = text.indexOf('|')
    if (p < 0) p = text.length
    n.setSelectionRange(p, p)
    n.selectionEnd = p
    ctx.$.textcomplete("trigger")
  }

  def suggestions(implicit ctx: TestCtx): Vector[String] = {
    // println(Sizzle(".textcomplete-item").headOption.fold("NADA!")(_.outerHTML))
    allTextCompleteULs
      .filterNot(_.style.display == "none")
      .lastOption
      .map(ul => Sizzle(".textcomplete-item a " + ctx.acDomSel, ul).map(_.textContent).toVector)
      .getOrElse(Vector.empty)
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
    keydown(KeyCode.Enter)
    val i = exp.indexOf('|')
    if (i < 0)
      assertEq(quote(ctx.editor.state), quote(exp))
    else {
      assertEq(quote(ctx.editor.state), quote(exp.replace("|", "")))
      testCursorPos(i)
    }
  }

  def testCursorPos(pos: Int)(implicit ctx: TestCtx): Unit = {
    val n = ctx.editor.getDOMNode.map(_.asElement)
    assertEq((pos, pos), (n.selectionStart, n.selectionEnd))
  }

  // ===================================================================================================================

  lazy val acReqItems = AutoComplete.Project.reqItems(project, plainText)
  lazy val acReqC     = AutoComplete.Project.req(textSearch, acReqItems, Contextualise)
  lazy val cReqC      = editor(acReqC)

  lazy val cReqCodePrefixes = editor(AutoComplete.Project.reqCode.prefixes(fakeTrie))
  lazy val cReqCodeRefs     = editor(AutoComplete.Project.reqCode.ref(project2, plainText2))

  lazy val cIssuesC = editor(AutoComplete.Project.issue(project.config.customIssueTypes.values.toStream, HideDead)(Contextualise))

  lazy val cTagsC = editor(AutoComplete.Project.tag(project.config.atagIterator.toStream, HideDead)(Contextualise))
  lazy val cTagsP = editor(AutoComplete.Project.tag(project.config.atagIterator.toStream, HideDead)(Plain))

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
    def tgt: ReqCode.Data = ReqCode.ActiveReq(nextId(), 1, None, ReqCode.emptyReqInactive)
    val t1 = codes.foldLeft(ReqCode.Trie.empty)((t, c) => t.put(c, tgt))

    def tomb = ReqCode.Data.empty.copy(deadGroup = Some(DeadCodeGroup(nextId(), "asdf")))
    val tombCodes = Set[ReqCode.Value](
      "apple.dead", "ahhdead", "dead.eggs"
    )
    tombCodes.foldLeft(t1)((t, c) => t.put(c, tomb))
  }
  lazy val project2 = {
    import ProjectDsl._, UnsafeTypes._
    val p = Project.reqCodes.set(ReqCodes(fakeTrie))(SampleProject2.project)
    (DeadReqCode("dead.ref", oldReqId = 1, id = 90) + DeadReqCode("dead.group", id = 91)) ! p
  }
  lazy val plainText2 = PlainText.ForProject.noCtx(project2)

  override def tests = Tests {

    'issue {
      implicit val ctx = TestCtx(cIssuesC)

      'start {
        test("#T")("TBD", "TO"+"DO")
        testSelect("#TBD")
      }
      'mid {
        test("#DO")("TO"+"DO")
        testSelect("#TO"+"DO")
      }
      'noSyntax -
        test("T")()
      'filterDead -
        test("#D")("TBD", "TO"+"DO") // PENDING is dead
    }

    'tagC {
      implicit val ctx = TestCtx(cTagsC)

      'start {
        test("#pri")("pri=high", "pri=low", "pri=med")
        testSelect("#pri=high")
      }
      'mid {
        test("#1")("v1.0", "v1.1", "v1.2", "v1.3", "v1.x")
        testSelect("#v1.0")
      }
      'noSyntax -
        test("pri")()
      'filterDead -
        test("#x")("v1.x", "v2.x") // v3.x & v4.x are dead
    }

    'tagP {
      implicit val ctx = TestCtx(cTagsP)

      'start {
        test("pri")("pri=high", "pri=low", "pri=med")
        testSelect("pri=high")
      }
      'mid {
        test("1")("v1.0", "v1.1", "v1.2", "v1.3", "v1.x")
        testSelect("v1.0")
      }
      'withSyntax - {
        test("#pri")("pri=high", "pri=low", "pri=med")
        testSelect("pri=high")
      }
      'filterDead -
        test("#x")("v1.x", "v2.x") // v3.x & v4.x are dead
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
      implicit val ctx = TestCtx(editor(AutoComplete.Project.math))
      test("<m")("math")
      testSelect("<math>|</math>")
      test("before <m|")("math")
      testSelect("before <math>|</math>")
    }
  }
}
*/