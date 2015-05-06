package shipreq.webapp.base.text

import java.util.concurrent.atomic.AtomicInteger
import japgolly.nyaya._
import japgolly.nyaya.util._
import japgolly.nyaya.test._
import japgolly.nyaya.test.PropTest._
import org.parboiled2._
import shipreq.base.util.{NonEmptyVector, UnivEq}
import scala.util.{Try, Failure, Success}
import scalaz.Equal
import scalaz.std.list._
import scalaz.std.string._
import scalaz.std.stream._
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.{RandomData => $}
import shipreq.webapp.base.test.{SampleProject, ProjectDSL, UnsafeTypes}
import shipreq.webapp.base.test.BaseTestUtil._
import Text.Equality._
import Atom.AnyAtom

object ParsersTest extends TestSuite {

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

  class Tester(p: Project, inputs: List[String]) {
    override def toString = p.toString

//    println(p.countAtoms.showTree + "\n")

    val E = EvalOver(this)

    val txt2str = PlainText(p).format

    val genericReqTitles = p.reqs.data.reqs.values.filterT[GenericReq].map(_.title)

    val customTextFieldValues = p.reqFieldData.data.text.values.toStream.flatMap(_.values.toStream)

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
//        p.customIssueTypes.data.values.toStream.map(_.toString).sorted foreach println
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

    def testString(in0: String) = {
      val in1  = String valueOf Parsers.preprocess(in0, multiLine = true)
      val parser = Text.CustomTextField.parser(p)(in1)
      val out1 = assertSuccess(parser, parser.optionalText.run())
      val in2  = txt2str(out1)
      val out2 = Text.CustomTextField.parse(p)(in2)
      count(out1)
      cmp(in1, out1, out2)
    }

    def all = (
        E.forall(genericReqTitles)(testGenericReqTitle).rename("GenericReqDesc")
      ∧ E.forall(customTextFieldValues)(testCustomTextField).rename("CustomTextField")
      ∧ E.forall(inputs)(testString).rename("parse |> toStr |> parse = parse")
      )
  }

  def tester: Gen[Tester] =
    for {p <- $.project; ss <- $.TextGen.genstr1.list1.map(_.list)} yield new Tester(p, ss)

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
    import ProjectDSL._
    import UnsafeTypes._
    val List(co,mf,fr) = List[ReqTypeId](1,2,3).map(Some(_))
    GReq(reqType = fr, id = 11, title = "do stuff") +
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

  // TODO ReqTitle doesn't allow tags. Rethink this?

  override val tests = TestSuite {
    'manual {
      import UnsafeTypes._

      def testT[A <: AnyAtom](p: Project, parse: Project => String => Vector[A], text: String)(as: A*): Unit = {
        val e = as.toVector
        assertEq(parse(p)(text), e)
        val text2 = PlainText(p).format(e)
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
        'empty  - test("    ")()
        'lit    - test("  hehe  ")(L("hehe"))
        'email  - test("  asd@abc.com  ")(T.EmailAddress("asd@abc.com"))
        'li     - test("*     hehe    \n*     yay    ")(T.UnorderedList(NEV(LI(L("hehe")), LI(L("yay")))))
        'nl     - test("here\nthere")(L("here"), T.blankLine, L("there"))
        'nls    - test("here \n \n\n there")(L("here"), T.blankLine, L("there"))
        'listNL - test("ok\n\n\n*   hehe \n \n\n  \n *  yay \n\n\n bye")(L("ok"), T.UnorderedList(NEV(LI(L("hehe")), LI(L("yay")))), L("bye"))
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
    }

    'small {
      'emailAddress - $.TextGen.emailAddress(T).mustSatisfy(propEmailAddress)
      'webAddress   - $.TextGen.webAddress  (T).mustSatisfy(propWebAddress)
      'mathtex      - $.TextGen.mathTex     (T).mustSatisfy(propMathTeX)
    }

    'big {
      tester.mustSatisfyE(_.all) //(DefaultSettings.propSettings.setSampleSize(1000).setSeed(100).setGenSize(100))
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
