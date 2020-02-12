package shipreq.webapp.base.text

import japgolly.microlibs.testutil.TestUtilInternals.quoteStringForDisplay
import java.util.concurrent.atomic.AtomicInteger
import nyaya.prop._
import nyaya.gen._
import nyaya.util._
import nyaya.test._
import nyaya.test.PropTest._
import org.parboiled2._
import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import scala.util.{Try, Failure, Success}
import scalaz.Equal
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.{RandomData => $}
import shipreq.webapp.base.test.{ProjectDsl, UnsafeTypes}
import shipreq.webapp.base.test.{SampleProject6 => SP}
import shipreq.webapp.base.test.WebappTestUtil._
import Atom.AnyAtom

object ParsersTest extends TestSuite {

  def preprocessStr(s: String, lc: LineCardinality): String =
    Parsers.preProcessor(lc)(s).asString

  val counts = Atom.Type.values.iterator.map((_, new AtomicInteger)).toMap
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
//    p.content.reqCodes.trie.foreachPathAndValue((c,d) => println(s"${d.activeId.fold("-")(_.value.toString)} : ${PlainText reqCode c} - $d"))
//    println()

    val E = EvalOver(this)

    val txt2str = PlainText.ForProject.noCtx(p).text(_: Text.AnyOptional, Live, Mandatory.Not)

    val genericReqTitles =
      p.content.reqs.reqIterator
        .filterSubType[GenericReq]
        //.filter(_.live(p.config.reqTypes) :: Live)
        .map(_.title)
        .toList

    val customTextFieldValues =
      p.content.reqText.values.toStream.flatMap(_.values.toStream)

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
      E.equal(t.take(2000), a, e)
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
      val parser = Text.GenericReqTitle.parser(p, None)(txt)
      val parsed = assertSuccess(parser, parser.optionalText.run())
      cmp("[GenericReqDesc] toStr |> parse = id", parsed, src)
    }

    def testCustomTextField(t: Text.CustomTextField.NonEmptyText) = {
      val src = t.whole
      count(src)
      val txt = txt2str(src)
      val parsed = Text.CustomTextField.parse(p, None)(txt)
      cmp(s"[CustomTextField] toStr |> parse = id\n${quoteStringForDisplay(txt)}", parsed, src)
    }

    def testStringML(in0: String) = {
      val in1    = preprocessStr(in0, MultiLine)
      val parser = Text.CustomTextField.parser(p, None)(in0) // in0, not in1, cos it should preprocess by itself
      val par1   = assertSuccess(parser, parser.optionalText.run())
      val in2    = txt2str(par1)
      val in3    = preprocessStr(in2, MultiLine)
      val par2   = Text.CustomTextField.parse(p, None)(in2)
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
    import SP.Values._

    // Create reqCodes that look like pubids
    GReq(reqType = co, title = "be fast").code("co2") +
    GReq(reqType = co, title = "be good").code("co1").code("here.i.am_3") !
    SP.project
  }
  @inline val V = Vector
  @inline def NEV[A](h: A, t: A*) = NonEmptyVector(h, t: _*)
  @inline def LI[A <: AnyAtom](as: A*) = as.toVector
  @inline def L(s: String) = T.Literal(s)

  val reqCode_co2      = ApReqCodeId(9)
  val reqCode_co1      = ApReqCodeId(10)
  val reqCode_hereiam3 = ApReqCodeId(11)

  def propEmailAddress = parserProp("EmailAddress",
    (_: T.EmailAddress).value, T.parserI(P, None))(_.emailAddress.run())

  def propWebAddress = parserProp("WebAddress",
    (_: T.WebAddress).value, T.parserI(P, None))(_.webAddress.run())

  def propMathTeX = parserProp("MathTeX",
    (_: T.TeX).value |> Grammar.texSurround.display, T.parserI(P, None))(_.tex.run())

  val whitespaceCombos: Set[String] = {
    val chars = List(' ', '\n', '\r', '\t')
    val words = (1 to 3).iterator.flatMap(n => chars.combinations(n).flatMap(_.permutations)).map(_.mkString)
    words.toSet
  }

  val optBool: List[Option[Boolean]] =
    None :: Some(true) :: Some(false) :: Nil

  val maybeSpace = List("", " ")

  override val tests = Tests {
    'preprocess {
      // This isn't a standard trim - see preprocess() for explanation
      def post(s: String) = ">" + s.replace('\n', '_') + "<"
      val g = Gen.chooseGen(Gen.alphaNumeric, Gen pure '\n').string(0 to 20)
      val p = Prop.equal[String]("trim")(
        s => post(Parsers.preProcessor(MultiLine)(s).asString),
        s => post(s.trim))
      p mustBeSatisfiedBy g
    }

    'manual {
      import SP.Values._
      import UnsafeTypes._

      def testT[A <: AnyAtom](p: Project, parse: Project => String => Vector[A], text: String)(as: A*): Unit = {
        val e = as.toVector
        assertEq(quoteStringForDisplay(preprocessStr(text, MultiLine)), parse(p)(text), e)

//        def x[B](as: Vector[B]) = as.mkString("\n")
//        def x[B](as: Vector[B]) = as.toString().replaceAll("(?<=[,\\(]) *(?!\\))", "\n")
//        assertMultiline(x(parse(p)(text)), x(e))

        val text2 = PlainText.ForProject.noCtx(p).text(e, Live, Mandatory.Not)
        assertEq(s"txt -> parsed -> txt:\n$text2", parse(p)(text2), e)
      }

      def test(text: String)(as: T.Atom*): Unit = {
        testWithUcCtx(text, None)(as: _*)
        testWithUcCtx(text, Some(1))(as: _*)
        testWithUcCtx(text, Some(99999))(as: _*)
      }

      def testWithUcCtx(text: String, currentUC: Option[ReqTypePos])(as: T.Atom*): Unit =
        testT(P, T.parse(_, currentUC), text)(as: _*)

      def testLit(text: String): Unit =
        test(text)(T.Literal(text))

      'hashHashHash -
        test("#v1.x#v1.0#TBD#TBD{ whatever}#pri=high")(
          T.TagRef(21), T.TagRef(22), T.Issue(2, V.empty), T.Issue(2, Vector(I.Literal("whatever"))), T.TagRef(2))

      'innerBraceInIssueDesc -
        test(s"#TBD{ <${Grammar.texTag}>\\frac{22}</${Grammar.texTag}> }")(T.Issue(2, Vector(I.TeX("\\frac{22}"))))

      'whitespace {
        'empty   - test("    ")()
        'lit     - test("  hehe  ")(L("hehe"))
        'email   - test("  asd@abc.com  ")(T.EmailAddress("asd@abc.com"))
        'li      - test("*     hehe    \n*     yay    ")(T.UnorderedList(NEV(LI(L("hehe")), LI(L("yay")))))
        'nl      - test("here\nthere")(L("here"), T.blankLine, L("there"))
        'nls     - test("here \n \n\n there")(L("here"), T.blankLine, L("there"))
        'listNL  - test("ok\n\n\n*   hehe \n \n\n  \n *  yay \n\n\nbye")(L("ok"), T.UnorderedList(NEV(LI(L("hehe")), LI(L("yay")))), L("bye"))
        'codeRef - test("[ here . i . am_3 ]")(T.CodeRef(reqCode_hereiam3))
        'headNL  - whitespaceCombos.foreach(w => test(w + "good")(T.Literal("good")))
        'tailNL  - whitespaceCombos.foreach(w => test("good" + w)(T.Literal("good")))
      }

      'list {
        'empty - test("* ")(T.UnorderedList(NEV(LI())))
        'empties - test("* \n* ")(T.UnorderedList(NEV(LI(), LI())))
        'mid - test("a* b")(L("a* b"))
        'between - test("before\n* mid\nafter")(L("before"), T.UnorderedList(NEV(LI(L("mid")))), L("after"))
        'between2 - test("before\n* mid\n after")(L("before"), T.UnorderedList(NEV(LI(L("mid"), T.blankLine, L("after")))))
        'between3 - test("before\n* mid \n\n \n after")(L("before"), T.UnorderedList(NEV(LI(L("mid"), T.blankLine, L("after")))))
        'between4 - test("before\n* mid\n     \n\n      \nafter")(L("before"), T.UnorderedList(NEV(LI(L("mid")))), L("after"))

        'newlines - test(
          """
            |* a1
            |  a2
            |* b
            |
            |* c1
            |
            |   c2
            |
            |
            |    c3
            |
            |
            |
            |*  d
            |*    e1
            |
            |
            |
            |  e2
            |
            |
            |yo
            |""".stripMargin)(
          T.UnorderedList(NEV(
            LI(L("a1"), T.blankLine, L("a2")),
            LI(L("b")),
            LI(L("c1"), T.blankLine, L("c2"), T.blankLine, L("c3")),
            LI(L("d")),
            LI(L("e1"), T.blankLine, L("e2")),
          )),
          L("yo"))

        'indents - test(
          """
            |  * a1
            |  a2
            | a3
            |  * b
            |omg
            |    * c1
            |      c2
            |             c3
            |    * d1
            | *d2
            |
            |    * e1
            |
            |  e2
            |
            |* ok
            |ah
            |""".stripMargin)(
          T.UnorderedList(NEV(
            LI(L("a1"), T.blankLine, L("a2"), T.blankLine, L("a3")),
            LI(L("b")),
          )),
          L("omg"),
          T.UnorderedList(NEV(
            LI(L("c1"), T.blankLine, L("c2"), T.blankLine, L("c3")),
            LI(L("d1"), T.blankLine, L("*d2")),
            LI(L("e1"), T.blankLine, L("e2")),
            LI(L("ok")),
          )),
          L("ah"))

        'bullets - test(
          """
            |Q
            |    • A
            |    • B
            |    • C
            |R
            |    •A
            |    •B
            |    •C
            |S
            |    * A
            |    * B
            |    * C
            |T
            |    *A
            |    *B
            |    *C
            |U
            |""".stripMargin)(
          L("Q"),
          T.UnorderedList(NEV(
            LI(L("A")),
            LI(L("B")),
            LI(L("C")),
          )),
          L("R"),
          T.UnorderedList(NEV(
            LI(L("A")),
            LI(L("B")),
            LI(L("C")),
          )),
          L("S"),
          T.UnorderedList(NEV(
            LI(L("A")),
            LI(L("B")),
            LI(L("C")),
          )),
          L("T"),
          T.blankLine, L("*A"),
          T.blankLine, L("*B"),
          T.blankLine, L("*C"),
          T.blankLine, L("U"))
      }

      'codeBlocks - {
        'flat - test(
          """
            |```
            |
            |ok
            |
            |
            |  1
            |
            |
            |cool
            |
            |```
            |hello
            |```
            |* here we go again!
            |```
            |
            |
            |hello again
            |
            |
            |```
            | whee
            |```
            |""".stripMargin.trim)(
          T.CodeBlock("ok\n\n\n  1\n\n\ncool"), // blank lines trimmed
          L("hello"),
          T.CodeBlock("* here we go again!"), // blank lines after block removed
          L("hello again"),
          T.CodeBlock(" whee"), // blank lines before block removed
        )

        'inList - test(
          """
            |* ```
            |ok
            |
            |  great
            |  ```
            |
            |*  ```
            |
            |     hey
            |
            |    ```
            |
            |* cool
            |  ```
            |    good job, me
            |  ```
            |* omfg
            |
            |
            |
            | ```
            |   derp
            |
            | ```
            |
            |   ahh
            |noice
            |""".stripMargin.trim)(
          T.UnorderedList(NEV(
            LI(T.CodeBlock("ok\n\n  great")),
            LI(T.CodeBlock("  hey")),
            LI(L("cool"), T.CodeBlock("  good job, me")),
            LI(L("omfg"), T.CodeBlock("  derp"), L("ahh")),
          )),
          L("noice")
        )

        'inList2 - test(
          """
            |* right
            |
            | ```
            | inner
            | ```
            |
            |```
            |outer
            |```
            | done
            |""".stripMargin.trim)(
          T.UnorderedList(NEV(
            LI(L("right"), T.CodeBlock("inner")),
          )),
          T.CodeBlock("outer"),
          L("done")
        )

        'beforeEmptyList - test(
          "```\nasd\n```\n* "
        )(
          T.CodeBlock("asd"),
          T.UnorderedList(NEV(LI())),
        )

        'empty - test(
          """
            |```
            |```
            |
            |```
            |
            |
            |
            |```
            |
            |* ```
            |  ```
            |
            |* here
            |
            |  ```
            |
            |  ```
            |
            |  ok
            |""".stripMargin
        )(
          T.CodeBlock(""),
          T.CodeBlock(""),
          T.UnorderedList(NEV(
            LI(T.CodeBlock("")),
            LI(L("here"), T.CodeBlock(""), L("ok")),
          )),
        )

        'indentedRoot - test(
          """
            |preventing trim
            |
            | ```
            | a
            |  a
            | ```
            |
            |  ```
            |    b
            |     b
            |  ```
            |
            | ```
            |c
            | c
            |  ```
            |
            |  ```
            |d
            | d
            | ```
            |
            |  ```
            |    e
            |   e
            | ```
            |
            | ```
            |    f
            |   f
            |  ```
            |""".stripMargin
        )(
          L("preventing trim"),
          T.CodeBlock("a\n a"),
          T.CodeBlock("  b\n   b"),
          T.CodeBlock("c\n c"),
          T.CodeBlock("d\n d"),
          T.CodeBlock("   e\n  e"),
          T.CodeBlock("   f\n  f"),
        )
      }

      'useCaseStepRef {
        def testU(id: UseCaseStepId, stepLabel: String): Unit = {
          val stepLabelUC = wrapString(stepLabel).takeWhile(Character.isDigit).toInt
          val expect = T.UseCaseStepRef(id)
          for {
            ucCtx    ← List[Option[ReqTypePos]](None, Some(1), Some(99999))
            useCtx   = ucCtx.exists(_.value ==* stepLabelUC)
            stepStr  = if (useCtx) wrapString(stepLabel).dropWhile(Character.isDigit).self else stepLabel
            prefix   ← if (useCtx) maybeSpace else List("", "UC-", "uc", " Uc - ")
            suffix   ← maybeSpace
            dotNoise ← null :: " ." :: ". " :: "  .  " :: Nil
            chCase   ← optBool
            padZero  ← false :: true :: Nil
          } {
            var s = stepStr
            chCase match {
              case Some(true)  => s = s.toLowerCase.replace(".x.", ".X.")
              case Some(false) => s = s.toUpperCase
              case None        => ()
            }
            if (dotNoise ne null) s = s.replace(".", dotNoise)
            if (padZero) s = s.replaceAll("(?=\\d+)", "0")
            s = "[" + prefix + s + suffix + "]"
            testWithUcCtx(s, ucCtx)(expect)
          }
        }

        'liveN1 - testU(11, step11_label)
        'liveN2 - testU(19, step19_label)
        'liveE1 - testU(18, step18_label)
        'liveE2 - testWithUcCtx("[E.1]", Some(1))(T.UseCaseStepRef(18))
        'deadN - testU(16, step16_label)
        'deadN - testU(20, step20_label)
        'deadE - testU(17, step17_label)

        'endInX - testLit("[1.0.X]")
        'negN1  - testLit("[1.-1]")
        'negN2  - testLit("[1.0.-1]")
        'negE1  - testLit("[1.E.-1]")
        // should also test some invalid combinations
      }

      'altForms {
        'req - test("[fr1][fr 1][ fr - 2 ][Mf-1 ]")(T.ReqRef(frs(1)), T.ReqRef(frs(1)), T.ReqRef(frs(2)), T.ReqRef(mfs(1)))
        'tag - test("#wip#DEFER#V3.x")(T.TagRef(11), T.TagRef(12), T.TagRef(26))
        'issue - test("#tbd{cool}#Todo#TBD { nice }")(
          T.Issue(2, Vector(I.Literal("cool"))), T.Issue(1, Vector.empty), T.Issue(2, Vector(I.Literal("nice"))))
      }

      'ambiguity {
        'pubid - test("[CO1][co-1]")(T.ReqRef(cos(1)), T.ReqRef(cos(1)))
        'code  - test("[co1][co2]")(T.CodeRef(reqCode_co1), T.CodeRef(reqCode_co2))
      }
    }

    'small {
      'emailAddress - $.TextGen.emailAddress(T).mustSatisfy(propEmailAddress)
      'webAddress   - $.TextGen.webAddress  (T).mustSatisfy(propWebAddress)
      'tex          - $.TextGen.tex         (T).mustSatisfy(propMathTeX)
    }

    // The [parse . toString = id] property doesn't hold with dead dead/alternate CodeRefs.
    // Eg. Dead text can have CodeRefs to dead codes.
    // Parsing text only happens to live text, and it only looks at active codes.
    'big {
//      tester.bugHunt(0, 10000)(Prop.eval(_.all))(DefaultSettings.propSettings.setSampleSize(1000).setSeed(1).setGenSize(4).setDebug.setSingleThreaded)
      tester.mustSatisfyE(_.all) //(DefaultSettings.propSettings.setSampleSize(20000).setDebug)
      println()
      val graphUnit = 1000 `JVM|JS` 10
      val graphChar = "#" `JVM|JS` "."
      println("Parser test distribution")
      println("=========================")
      Atom.Type.values.foreach { t =>
        val c = counts(t).get()
        printf("%-14s :%7d | %s\n", t.toString, c, graphChar * (c / graphUnit))
      }
      println("------------------------+")
      printf("%-14s :%7d |\n", "Total", counts.values.map(_.get).sum)
      println("------------------------+")
      println()
    }
  }
}
