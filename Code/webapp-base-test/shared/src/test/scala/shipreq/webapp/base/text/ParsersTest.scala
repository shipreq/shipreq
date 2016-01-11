package shipreq.webapp.base.text

import java.util.concurrent.atomic.AtomicInteger
import nyaya.prop._
import nyaya.gen._
import nyaya.util._
import nyaya.test._
import nyaya.test.PropTest._
import org.parboiled2._
import shipreq.base.util.NonEmptyVector
import scala.util.{Try, Failure, Success}
import scalaz.Equal
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.{RandomData => $}
import shipreq.webapp.base.test.{SampleProject, ProjectDsl, UnsafeTypes}
import shipreq.webapp.base.test.WebappTestUtil._
import Atom.AnyAtom

object ParsersTest extends TestSuite {

  def quoteStr(s: String): String =
    s"[${s.replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")}]"

  def preprocessStr(s: String, multiLine: Boolean): String =
    String valueOf Parsers.preprocess(s, multiLine)

  val counts = Atom.Type.values.toStream.map((_, new AtomicInteger)).toMap
  def count(as: Iterable[AnyAtom]): Unit =
    as.foreach { a =>
      val t = Atom.Type of a
      counts(t).incrementAndGet()
    }

  def assertSuccess[A](parser: Parser, t: Try[A]): A =
    t match {
      case Success(a) => a
      case Failure(e: ParseError) =>
        fail(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) =>
        fail(e.toString)
    }

  class Tester(p: Project, inputsML: List[String]) {
    override def toString = p.toString

//    println(p.countAtoms.showTree + "\n")
//    println()
//    p.reqCodes.trie.foreachPathAndValue((c,d) => println(s"${d.activeId.fold("-")(_.value.toString)} : ${PlainText reqCode c} - $d"))
//    println()

    val E = EvalOver(this)

    val txt2str = PlainText(p).format(Live, _: Text.AnyOptional)

    val genericReqTitles =
      p.reqs.reqs.values
        .filterT[GenericReq]
        //.filter(_.live(p.config.customReqTypes) :: Live)
        .map(_.title)

    val customTextFieldValues =
      p.reqText.values.toStream.flatMap(_.values.toStream)

    def cmp[A <: AnyAtom](t: => String, actual0: Iterable[A], expect0: Iterable[A]): EvalL = {

      val actual = actual0.toVector
      val expect = expect0.toVector
      var a = actual
      var e = expect
      while (a.nonEmpty && e.nonEmpty && a.head == e.head) {
        a = a.tail
        e = e.tail
      }
//      while (a.nonEmpty && e.nonEmpty && a.last == e.last) {
//        a = a.init
//        e = e.init
//      }

//      if (a != e) debug(t)
      E.equal(t.takeRight(200), a, e)
//       E.equal(t.takeRight(200), actual, expect)
    }

//    var first = true
//    def debug(t: String) = {
//      if (first) {
//        first = false
//        println(">"*200)
//        println()
//        println(t)
//        println()
//        p.config.customIssueTypes.values.toStream.map(_.toString).sorted foreach println
//        println()
//        println("<"*200)
//      }
//    }

    def testGenericReqTitle(src: Text.GenericReqTitle.OptionalText) = {
      count(src)
      val txt = txt2str(src)
      val parser = Text.GenericReqTitle.parser(p)(txt)
      val parsed = assertSuccess(parser, parser.optionalText.run())
      cmp("[GenericReqDesc] toStr |> parse = id", parsed, src)
    }

    def testCustomTextField(t: Text.CustomTextField.NonEmptyText) = {
      val src = t.whole
      count(src)
      val txt = txt2str(src)
      val parsed = Text.CustomTextField.parse(p)(txt)
      cmp(s"[CustomTextField] toStr |> parse = id\n'''$txt'''", parsed, src)
    }

    def testStringML(in0: String) = {
      val in1    = preprocessStr(in0, multiLine = true)
      val parser = Text.CustomTextField.parser(p)(in0) // in0, not in1, cos it should preprocess by itself
      val par1   = assertSuccess(parser, parser.optionalText.run())
      val in2    = txt2str(par1)
      val in3    = preprocessStr(in2, multiLine = true)
      val par2   = Text.CustomTextField.parse(p)(in2)
//      if (in2 startsWith "\n")
//        println(
//          List(
//            "in0" -> in0,
//            "in1" -> in1,
//            "par1" -> par1.toString,
//            "in2" -> in2,
//            "in3" -> in3,
//            "par2" -> par2.toString)
//            .map{case (k,v) => s"$k: [${v.replace("\n", "↲")}]"}.mkString("\n") + "\n")
      count(par1)
      cmp(in1, par1, par2).rename("parse |> toStr |> parse = parse") ∧
      Eval.equal("txt2str |> preprocess = txt2str", in0, in2, in3) ∧
      DataProp.text.anyText(par1).liftL.rename("DataProp.anyText")
    }

    def all = (
        E.forall(genericReqTitles)(testGenericReqTitle).rename("GenericReqDesc")
      ∧ E.forall(customTextFieldValues)(testCustomTextField).rename("CustomTextField")
      ∧ E.forall(inputsML)(testStringML).rename("Parse inputs (multiline)")
      )
  }

  def tester: Gen[Tester] =
    Gen.lift2($.project, $.TextGen.genCharML.string1.list1)(new Tester(_, _))

  // -------------------------------------------------------------------------------------------------------------------

  def parserProp[A, P <: Parser](name: => String, txt: A => String, parser: ParserInput => P)(run: P => Try[A])(implicit eq: Equal[A]) =
    Prop.atom[A](name,
      a => {
        val p = parser(txt(a))
        val r = run(p)
        r match {
          case Success(b) =>
            if (eq.equal(a, b)) None else Some(s"Expected $a, got $b")
          case Failure(e: ParseError) =>
            Some(p.formatError(e, new ErrorFormatter(showTraces = true)))
          case Failure(e) =>
            Some(e.toString)
        }
      }
    )

  import Text.{CustomTextField => T, InlineIssueDesc => I}
  val P = {
    import ProjectDsl._
    import UnsafeTypes._
    import SampleProject.Values._
    GReq(reqType = co, id = 31, title = "be fast").code("co2") + // code inversed: [CO-1] [co2] ReqCodeId = 1
    GReq(reqType = co, id = 32, title = "be good").code("co1") + // code inversed: [CO-2] [co1] ReqCodeId = 2
    GReq(reqType = fr, id = 11, title = "do stuff").code("here.i.am_3") +
    GReq(reqType = fr, id = 12, title = "do more stuff") +
    GReq(reqType = mf, id = 21, title = "Use Case Editor") +
    GReq(reqType = mf, id = 22, title = "Templates") +
    GReq(reqType = mf, id = 23, title = "Incompletions") ! SampleProject.project
  }
  @inline val V = Vector
  @inline def NEV[A](h: A, t: A*) = NonEmptyVector(h, t: _*)
  @inline def LI[A <: AnyAtom](as: A*) = as.toVector
  @inline def L(s: String) = T.Literal(s)

  def propEmailAddress = parserProp("EmailAddress",
    (_: T.EmailAddress).value, T.parserI(P))(_.emailAddress.run())

  def propWebAddress = parserProp("WebAddress",
    (_: T.WebAddress).value, T.parserI(P))(_.webAddress.run())

  def propMathTeX = parserProp("MathTeX",
    (_: T.MathTeX).value |> Grammar.mathTexSurround.display, T.parserI(P))(_.mathtex.run())

  val whitespaceCombos: Set[String] = {
    val chars = List(' ', '\n', '\r', '\t')
    val words = (1 to 3).iterator.flatMap(n => chars.combinations(n).flatMap(_.permutations)).map(_.mkString)
    words.toSet
  }

  override val tests = TestSuite {
    'preprocess {
      // This isn't a standard trim - see preprocess() for explanation
      def post(s: String) = ">" + s.replace('\n', '_') + "<"
      val g = Gen.chooseGen(Gen.alphaNumeric, Gen pure '\n').string(0 to 20)
      val p = Prop.equal[String]("trim")(
        s => post(String.valueOf(Parsers.preprocess(s, true))),
        s => post(s.trim))
      p mustBeSatisfiedBy g
    }

    'manual {
      import UnsafeTypes._

      def testT[A <: AnyAtom](p: Project, parse: Project => String => Vector[A], text: String)(as: A*): Unit = {
        val e = as.toVector
        assertEq(quoteStr(preprocessStr(text, true)), parse(p)(text), e)
        val text2 = PlainText(p).format(Live, e)
        assertEq(text2, parse(p)(text2), e)
      }

      def test(text: String)(as: T.Atom*): Unit =
        testT(P, T.parse, text)(as: _*)

      'hashHashHash -
        test("#v1.x#v1.0#TBD#TBD{ whatever}#pri=high")(
          T.TagRef(21), T.TagRef(22), T.Issue(2, V.empty), T.Issue(2, Vector(I.Literal("whatever"))), T.TagRef(2))

      'innerBraceInIssueDesc -
        test("#TBD{ <math>\\frac{22}</math> }")(T.Issue(2, Vector(I.MathTeX("\\frac{22}"))))

      'whitespace {
        'empty   - test("    ")()
        'lit     - test("  hehe  ")(L("hehe"))
        'email   - test("  asd@abc.com  ")(T.EmailAddress("asd@abc.com"))
        'li      - test("*     hehe    \n*     yay    ")(T.UnorderedList(NEV(LI(L("hehe")), LI(L("yay")))))
        'nl      - test("here\nthere")(L("here"), T.blankLine, L("there"))
        'nls     - test("here \n \n\n there")(L("here"), T.blankLine, L("there"))
        'listNL  - test("ok\n\n\n*   hehe \n \n\n  \n *  yay \n\n\n bye")(L("ok"), T.UnorderedList(NEV(LI(L("hehe")), LI(L("yay")))), L("bye"))
        'codeRef - test("[ here . i . am_3 ]")(T.CodeRef(3))
        'headNL  - whitespaceCombos.foreach(w => test(w + "good")(T.Literal("good")))
        'tailNL  - whitespaceCombos.foreach(w => test("good" + w)(T.Literal("good")))
      }

      'list {
        'empty - test("* ")(T.UnorderedList(NEV(LI())))
        'between - test("before\n* mid\nafter")(L("before"), T.UnorderedList(NEV(LI(L("mid")))), L("after"))
      }

      'altForms {
        'req - test("[fr1][fr 1][ fr - 2 ][Mf-1 ]")(T.ReqRef(11), T.ReqRef(11), T.ReqRef(12), T.ReqRef(21))
        'tag - test("#wip#DEFER#V3.x")(T.TagRef(11), T.TagRef(12), T.TagRef(26))
        'issue - test("#tbd{cool}#Todo#TBD { nice }")(
          T.Issue(2, Vector(I.Literal("cool"))), T.Issue(1, Vector.empty), T.Issue(2, Vector(I.Literal("nice"))))
      }

      'ambiguity {
        'pubid - test("[CO1][co-1]")(T.ReqRef(31), T.ReqRef(31))
        'code  - test("[co1][co2]")(T.CodeRef(2), T.CodeRef(1)) // codes inversed
      }
    }

    'small {
      'emailAddress - $.TextGen.emailAddress(T).mustSatisfy(propEmailAddress)
      'webAddress   - $.TextGen.webAddress  (T).mustSatisfy(propWebAddress)
      'mathtex      - $.TextGen.mathTex     (T).mustSatisfy(propMathTeX)
    }

    // The [parse . toString = id] property doesn't hold with dead dead/alternate CodeRefs.
    // Eg. Dead text can have CodeRefs to dead codes.
    // Parsing text only happens to live text, and it only looks at active codes.
    'big {
//      tester.bugHunt(0, 10000)(Prop eval (_.all)) //(DefaultSettings.propSettings.setSampleSize(1000).setSeed(1).setGenSize(4).setDebug.setSingleThreaded)
      tester.mustSatisfyE(_.all) //(DefaultSettings.propSettings.setSampleSize(20000).setDebug)
      println()
      val graphUnit = 1000 `JVM|JS` 10
      val graphChar = "#" `JVM|JS` "."
      println("Parser test distribution")
      println("========================")
      Atom.Type.values.foreach { t =>
        val c = counts(t).get()
        printf("%-13s :%7d | %s\n", t.toString, c, graphChar * (c / graphUnit))
      }
      println("-----------------------+")
      printf("%-13s :%7d |\n", "Total", counts.values.map(_.get).sum)
      println("-----------------------+")
      println()
    }
  }
}
