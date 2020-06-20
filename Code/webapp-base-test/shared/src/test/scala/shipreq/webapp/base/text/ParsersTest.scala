package shipreq.webapp.base.text

import io.circe.syntax._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.testutil.TestUtilInternals.quoteStringForDisplay
import java.util.concurrent.atomic.AtomicInteger
import nyaya.gen._
import nyaya.prop.{Atom => _, _}
import nyaya.test.PropTest._
import nyaya.util._
import org.parboiled2._
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import scalaz.{Applicative, Equal}
import shipreq.base.test._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{NonEmptyArraySeq, Valid}
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.{ApplicableTagGD, Event}
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.{ProjectDsl, SampleProject6 => SP, TextShrink, UnsafeTypes}
import shipreq.webapp.base.text.Atom.AnyAtom
import shipreq.webapp.base.{RandomData => $}
import sourcecode.Line
import utest._

object ParsersTest extends TestSuite {
//  shipreq.webapp.base.RandomDataSettings.disableUnicode = true

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

    val txt2str = PlainText.ForProject.noCtx(p).text(_: Text.AnyOptional, Live, Optional)

    val genericReqTitles =
      p.content.reqs.reqIterator()
        .filterSubType[GenericReq]
        //.filter(_.live(p.config.reqTypes) :: Live)
        .map(_.title)
        .toList

    val customTextFieldValues =
      p.content.reqText.data.values.iterator.flatMap(_.values).toList

    def cmp[A <: AnyAtom](t: => String, expect: ArraySeq[A])(f: ArraySeq[A] => ArraySeq[A]): EvalL = {

      val actual = f(expect)
      var a = actual
      var e = expect

      // These cause issues cos it can break the composition rules avoided in RandomData.TextGen.postProcessAtoms
//      while (a.nonEmpty && e.nonEmpty && a.head == e.head) {
//        a = a.tail
//        e = e.tail
//      }
//      while (a.nonEmpty && e.nonEmpty && a.last == e.last) {
//        a = a.init
//        e = e.init
//      }

      if (a == e)
        E.pass
      else {
        def size(): Double = e.toString.length + a.toString.length
        val sizeBefore = size()
        e = TextShrink(e)(ee => Valid.when(f(ee) == ee))
        a = f(e)
        val sizeAfter = size()

        def pairOfOutput(name: String, f: ArraySeq[A] => String): String = {
          val es = f(e)
          val as = f(a)
          if (es == as)
            s"""Expect & actual $name:
               |${quoteString(es)}
               |""".stripMargin.trim
          else
            s"""Expect $name:
               |${quoteString(es)}
               |
               |Actual $name:
               |${quoteString(as)}
               |""".stripMargin.trim
        }

        val errMsg =
          s"""====================================================================================================
             |${pairOfOutput("AST", _.mkString("[", ", ", "]"))}
             |
             |${pairOfOutput("text", txt2str)}
             |
             |Shrink rate: ${((1.0 - sizeAfter / sizeBefore) * 100).toInt}%
             |====================================================================================================
             |""".stripMargin

        E.fail(t, errMsg)
      }
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
      cmp("[GenericReqDesc] toStr |> parse = id", src) { src =>
        val txt = txt2str(src)
        val parser = Text.GenericReqTitle.parser(p, None)(txt)
        val parsed = assertSuccess(parser, parser.optionalText.run())
        parsed
      }
    }

    def testCustomTextField(t: Text.CustomTextField.NonEmptyText) = {
      val src = t.whole
      count(src)
      cmp("[CustomTextField] toStr |> parse = id", src) { src =>
        val txt = txt2str(src)
        val parsed = Text.CustomTextField.parse(p, None)(txt)
        parsed
      }
    }

    def testStringML(in0: String) = {
      val in1    = preprocessStr(in0, MultiLine)
      val parser = Text.CustomTextField.parser(p, None)(in0) // in0, not in1, cos it should preprocess by itself
      val par1   = assertSuccess(parser, parser.optionalText.run())
      val in2    = txt2str(par1)
      val in3    = preprocessStr(in2, MultiLine)
      count(par1)

      def parseToStringAndBack =
        cmp(in1, par1) { par1 =>
          val in2  = txt2str(par1)
          val par2 = Text.CustomTextField.parse(p, None)(in2)
          // if (in2 startsWith "\n")
          //   println(
          //     List(
          //       "in0" -> in0,
          //       "in1" -> in1,
          //       "par1" -> par1.toString,
          //       "in2" -> in2,
          //       "in3" -> in3,
          //       "par2" -> par2.toString)
          //       .map{case (k,v) => s"$k: [${v.replace("\n", "↲")}]"}.mkString("\n") + "\n")
          par2
        }.rename("parse |> toStr |> parse = parse")

      parseToStringAndBack ∧
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

  import Text.{CustomTextField => T, InlineIssueDesc => I, StyledInnerFull => S}
  val P = {
    import ProjectDsl._
    import UnsafeTypes._
    import SP.Values._

    // Create reqCodes that look like pubids
    GReq(reqType = co, title = "be fast").code("co2") +
    GReq(reqType = co, title = "be good").code("co1").code("here.i.am_3") !
    SP.project
  }
  @inline def NEA[A: ClassTag](h: A, t: A*) = NonEmptyArraySeq(h, t: _*)
  @inline def LI[A <: AnyAtom: ClassTag](as: A*) = as.to(ArraySeq)
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
    "preprocess" - {
      // This isn't a standard trim - see preprocess() for explanation
      def post(s: String) = ">" + s.replace('\n', '_') + "<"
      val g = Gen.chooseGen(Gen.alphaNumeric, Gen pure '\n').string(0 to 20)
      val p = Prop.equal[String]("trim")(
        s => post(Parsers.preProcessor(MultiLine)(s).asString),
        s => post(s.trim))
      p mustBeSatisfiedBy g
    }

    "manual" - {
      import SP.Values._
      import UnsafeTypes._

      def testT[A <: AnyAtom: ClassTag](p: Project, parse: Project => String => ArraySeq[A], text: String)(as: A*)(implicit l: Line): Unit = {
        val e = as.to(ArraySeq)
        assertEq(quoteStringForDisplay(preprocessStr(text, MultiLine)), parse(p)(text), e)

//        def x[B](as: Vector[B]) = as.mkString("\n")
//        def x[B](as: Vector[B]) = as.toString().replaceAll("(?<=[,\\(]) *(?!\\))", "\n")
//        assertMultiline(x(parse(p)(text)), x(e))

        val text2 = PlainText.ForProject.noCtx(p).text(e, Live, Optional)
        assertEq(s"txt -> parsed -> txt:\n$text2", parse(p)(text2), e)
      }

      def test(text: String)(as: T.Atom*)(implicit l: Line, p: Project = null): Unit = {
        testWithUcCtx(text, None)(as: _*)
        testWithUcCtx(text, Some(1))(as: _*)
        testWithUcCtx(text, Some(99999))(as: _*)
      }

      def testWithUcCtx(text: String, currentUC: Option[ReqTypePos])(as: T.Atom*)(implicit l: Line, p: Project = null): Unit =
        testT(Option(p).getOrElse(P), T.parse(_, currentUC), text)(as: _*)

      def testLit(text: String)(implicit l: Line): Unit =
        test(text)(T.Literal(text))

      "hashHashHash" -
        test("#v1.x#v1.0#TBD#TBD{ whatever}#pri=high")(
          T.TagRef(21), T.TagRef(22), T.Issue(2, I.empty), T.Issue(2, I(I.Literal("whatever"))), T.TagRef(2))

      "innerBraceInIssueDesc" -
        test(s"#TBD{ <${Grammar.texTag}>\\frac{22}</${Grammar.texTag}> }")(T.Issue(2, I(I.TeX("\\frac{22}"))))

      "whitespace" - {
        "empty"   - test("    ")()
        "lit"     - test("  hehe  ")(L("hehe"))
        "litMid"  - test("  he   he  ")(L("he he"))
        "email"   - test("  asd@abc.com  ")(T.EmailAddress("asd@abc.com"))
        "li"      - test("*     hehe    \n*     yay    ")(T.UnorderedList(NEA(LI(L("hehe")), LI(L("yay")))))
        "nl"      - test("here\nthere")(L("here"), T.blankLine, L("there"))
        "nls"     - test("here \n \n\n there")(L("here"), T.blankLine, L("there"))
        "listNL"  - test("ok\n\n\n*   hehe \n \n\n  \n *  yay \n\n\nbye")(L("ok"), T.UnorderedList(NEA(LI(L("hehe")), LI(L("yay")))), L("bye"))
        "codeRef" - test("[ here . i . am_3 ]")(T.CodeRef(reqCode_hereiam3))
        "headNL"  - whitespaceCombos.foreach(w => test(w + "good")(T.Literal("good")))
        "tailNL"  - whitespaceCombos.foreach(w => test("good" + w)(T.Literal("good")))
      }

      "monospace" - {
        @inline def M(s: String) = T.Monospace(s)
        "empty"     - test("``")(L("``"))
        "easy"      - test("`a`")(M("a"))
        "trim"      - test("` abc  `")(M(" abc  "))
        "blank"     - test("` ``  `")(M(" "), M("  "))
        "consec1"   - test("`")(L("`"))
        "consec2"   - test("``")(L("``"))
        "consec3"   - test("```")(L("```"))
        "consec4"   - test("````")(L("````"))
        "spaced2"   - test("` `")(M(" "))
        "spaced3"   - test("` ` `")(M(" "), L(" `"))
        "spaced4"   - test("` ` ` `")(M(" "), L(" "), M(" "))
        "three"     - test("`hi`lo`")(M("hi"), L("lo`"))
        "multiline" - test("`omg\ncool`")(L("`omg"), T.blankLine, L("cool`")) // divergence from markdown
        "escape"    - test("` \\ \\\\ \\` \\\\` `")(M(" \\ \\\\ \\"), L(" \\\\"), M(" ")) // divergence from markdown
      }

      "styling" - {
        "bold" - {
          @inline def SS(i1: S.Atom, in: S.Atom*) = T.Bold(NonEmptyArraySeq(i1, in: _*))
          "empty"   - test("****")(L("****"))
          "spaceM"  - test("** **")(L("** **"))
          "spaceL"  - test("** x**")(L("** x**"))
          "spaceR"  - test("**x ****x **")(SS(S.Literal("x ")), SS(S.Literal("x")))
          "words"   - test("**a b c**")(SS(S.Literal("a b c")))
          "nl"      - test("**a\nc**")(L("**a"), T.blankLine, L("c**"))
          "heading" - test("**# nope**")(SS(S.Literal("# nope")))
          "tag"     - test("**#pri=high**")(SS(S.TagRef(2)))
          "issue"   - test("**#TBD#TBD{ whatever}**")(SS(S.Issue(2, I.empty), S.Issue(2, I(I.Literal("whatever")))))
          "lit"     - test("hey **there** mate!")(L("hey "), SS(S.Literal("there")), L(" mate!"))
        }
        "italic" - {
          @inline def SS(i1: S.Atom, in: S.Atom*) = T.Italic(NonEmptyArraySeq(i1, in: _*))
          "empty"   - test("////")(L("////"))
          "spaceM"  - test("// //")(L("// //"))
          "spaceL"  - test("// x//")(L("// x//"))
          "spaceR"  - test("//x ////x //")(SS(S.Literal("x ")), SS(S.Literal("x")))
          "words"   - test("//a b c//")(SS(S.Literal("a b c")))
          "nl"      - test("//a\nc//")(L("//a"), T.blankLine, L("c//"))
          "heading" - test("//# nope//")(SS(S.Literal("# nope")))
          "tag"     - test("//#pri=high//")(SS(S.TagRef(2)))
          "issue"   - test("//#TBD#TBD{ whatever}//")(SS(S.Issue(2, I.empty), S.Issue(2, I(I.Literal("whatever")))))
          "lit"     - test("hey //there// mate!")(L("hey "), SS(S.Literal("there")), L(" mate!"))
        }
        "strikethrough" - {
          @inline def SS(i1: S.Atom, in: S.Atom*) = T.Strikethrough(NonEmptyArraySeq(i1, in: _*))
          "empty"   - test("~~~~")(L("~~~~"))
          "spaceM"  - test("~~ ~~")(L("~~ ~~"))
          "spaceL"  - test("~~ x~~")(L("~~ x~~"))
          "spaceR"  - test("~~x ~~~~x ~~")(SS(S.Literal("x ")), SS(S.Literal("x")))
          "words"   - test("~~a b c~~")(SS(S.Literal("a b c")))
          "nl"      - test("~~a\nc~~")(L("~~a"), T.blankLine, L("c~~"))
          "heading" - test("~~# nope~~")(SS(S.Literal("# nope")))
          "tag"     - test("~~#pri=high~~")(SS(S.TagRef(2)))
          "issue"   - test("~~#TBD#TBD{ whatever}~~")(SS(S.Issue(2, I.empty), S.Issue(2, I(I.Literal("whatever")))))
          "lit"     - test("hey ~~there~~ mate!")(L("hey "), SS(S.Literal("there")), L(" mate!"))
        }
        "underline" - {
          @inline def SS(i1: S.Atom, in: S.Atom*) = T.Underline(NonEmptyArraySeq(i1, in: _*))
          "empty"      - test("____")(L("____"))
          "spaceM"     - test("__ __")(L("__ __"))
          "spaceL"     - test("__ x__")(L("__ x__"))
          "spaceR"     - test("__x ____x __")(SS(S.Literal("x ")), SS(S.Literal("x")))
          "words"      - test("__a b c__")(SS(S.Literal("a b c")))
          "nl"         - test("__a\nc__")(L("__a"), T.blankLine, L("c__"))
          "heading"    - test("__# nope__")(SS(S.Literal("# nope")))
          "tag"        - test("__#pri=high ____#pri=high __")(SS(S.TagRef(2), S.Literal(" ")), SS(S.TagRef(2)))
          "tag0"       - test("__#pri=high__")(SS(S.TagRef(2)))
          "tagFirst"   - test("#pri=high__nice__")(T.TagRef(2), SS(S.Literal("nice")))
          "tagOnly1"   - test("#pri=high__")(T.TagRef(2), T.Literal("__"))
          "tagOnly2"   - test("#pri=high____")(T.TagRef(2), T.Literal("____"))
          "issue"      - test("__#TBD#TBD{ whatever}__")(SS(S.Issue(2, I.empty), S.Issue(2, I(I.Literal("whatever")))))
          "issueFirst" - test("#TBD__nice__")(T.Issue(2, I.empty), SS(S.Literal("nice")))
          "issueOnly1" - test("#TBD__")(T.Issue(2, I.empty), T.Literal("__"))
          "issueOnly2" - test("#TBD____")(T.Issue(2, I.empty), T.Literal("____"))
          "lit"        - test("hey __there__ mate!")(L("hey "), SS(S.Literal("there")), L(" mate!"))
          "emailOk1"   - test("a__b@c__d.io")(T.EmailAddress("a__b@c__d.io"))
        //"emailOk2"   - test("__b@c__d.io")(T.EmailAddress("__b@c__d.io"))
          "email"      - test("__x a__b@c__d.io __")(T.Underline(NonEmptyArraySeq(S.Literal("x a"))), T.EmailAddress("b@c__d.io"), T.Literal(" __"))
        }

        "combos" - {
          "seq" - test("__a__**b**~~c ~~ //d//")(
            T.Underline(NonEmptyArraySeq(S.Literal("a"))),
            T.Bold(NonEmptyArraySeq(S.Literal("b"))),
            T.Strikethrough(NonEmptyArraySeq(S.Literal("c "))),
            T.Literal(" "),
            T.Italic(NonEmptyArraySeq(S.Literal("d"))),
          )

          "ok1" - test("__**hehe**__")(
            T.Underline(NonEmptyArraySeq(
              S.Bold(NonEmptyArraySeq(
                S.Literal("hehe"))))))

          "ok2" - test("__**hehe** //it ~~seems~~ to work//__")(
            T.Underline(NonEmptyArraySeq(
              S.Bold(NonEmptyArraySeq(
                S.Literal("hehe"))),
              S.Literal(" "),
              S.Italic(NonEmptyArraySeq(
                S.Literal("it "),
                S.Strikethrough(NonEmptyArraySeq(
                  S.Literal("seems"))),
                S.Literal(" to work"))))))

          "self2" - test("__**hehe** **it __seems__ to work**__")(
            T.Underline(NonEmptyArraySeq(
              S.Bold(NonEmptyArraySeq(
                S.Literal("hehe"))),
              S.Literal(" "),
              S.Bold(NonEmptyArraySeq(
                S.Literal("it __seems__ to work"))))))

          "litMix" - test("x__u__y__U__z")(
            L("x"),
            T.Underline(NonEmptyArraySeq(
              S.Literal("u"),
            )),
            L("y"),
            T.Underline(NonEmptyArraySeq(
              S.Literal("U"),
            )),
            L("z"),
          )

          "ptm1" - test("**asd@asd.com**")(
            T.Bold(NonEmptyArraySeq(
              S.EmailAddress("asd@asd.com"),
            ))
          )

          "ptm1b" - test("__asd@as_d.co_m__")(
            T.Underline(NonEmptyArraySeq(
              S.EmailAddress("asd@as_d.co_m"),
            ))
          )

          "ptm1c" - test("~~x~~x@x.com")(
            T.Strikethrough(NonEmptyArraySeq(
              S.Literal("x"),
            )),
            T.EmailAddress("x@x.com"),
          )

          "ptm1w" - test("**email me at asd@asd.com ok!**")(
            T.Bold(NonEmptyArraySeq(
              S.Literal("email me at "),
              S.EmailAddress("asd@asd.com"),
              S.Literal(" ok!"),
            ))
          )

          "ptm2" - test("**__`mono!`__**")(
            T.Bold(NonEmptyArraySeq(
              S.Underline(NonEmptyArraySeq(
                S.Monospace("mono!"),
              ))
            ))
          )

          "ul" - {
            val id = ApplicableTagId(1237645)
            val zs = List("z", "Z")
            for {
              a <- zs
              b <- zs
            } {
              val vs = ApplicableTagGD(
                applicableReqTypes = ApplicableReqTypes.empty,
                children           = Vector.empty,
                colour             = None,
                desc               = None,
                key                = HashRefKey(a),
                parents            = Map.empty,
              )
              implicit val p = applyEventSuccessfully(P, Event.ApplicableTagCreate(id, vs))
              test(s"* __#${b}__")(
                T.UnorderedList(NonEmptyArraySeq(
                  ArraySeq(T.Underline(NonEmptyArraySeq(S.TagRef(id)))))))
            }
          }

          "innerWS" - test("**X  Y  ****X  Y  **")(
            T.Bold(NonEmptyArraySeq(S.Literal("X Y "))),
            T.Bold(NonEmptyArraySeq(S.Literal("X Y"))), // DataProp requires that last is not allowed to end in whitespace
          )
        }
      }

      "list" - {
        "empty" - test("* ")(T.UnorderedList(NEA(LI())))
        "empties" - test("* \n* ")(T.UnorderedList(NEA(LI(), LI())))
        "mid" - test("a* b")(L("a* b"))
        "between" - test("before\n* mid\nafter")(L("before"), T.UnorderedList(NEA(LI(L("mid")))), L("after"))
        "between2" - test("before\n* mid\n after")(L("before"), T.UnorderedList(NEA(LI(L("mid"), T.blankLine, L("after")))))
        "between3" - test("before\n* mid \n\n \n after")(L("before"), T.UnorderedList(NEA(LI(L("mid"), T.blankLine, L("after")))))
        "between4" - test("before\n* mid\n     \n\n      \nafter")(L("before"), T.UnorderedList(NEA(LI(L("mid")))), L("after"))

        "newlines" - test(
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
          T.UnorderedList(NEA(
            LI(L("a1"), T.blankLine, L("a2")),
            LI(L("b")),
            LI(L("c1"), T.blankLine, L("c2"), T.blankLine, L("c3")),
            LI(L("d")),
            LI(L("e1"), T.blankLine, L("e2")),
          )),
          L("yo"))

        "indents" - test(
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
          T.UnorderedList(NEA(
            LI(L("a1"), T.blankLine, L("a2"), T.blankLine, L("a3")),
            LI(L("b")),
          )),
          L("omg"),
          T.UnorderedList(NEA(
            LI(L("c1"), T.blankLine, L("c2"), T.blankLine, L("c3")),
            LI(L("d1"), T.blankLine, L("*d2")),
            LI(L("e1"), T.blankLine, L("e2")),
            LI(L("ok")),
          )),
          L("ah"))

        "bullets" - test(
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
          T.UnorderedList(NEA(
            LI(L("A")),
            LI(L("B")),
            LI(L("C")),
          )),
          L("R"),
          T.UnorderedList(NEA(
            LI(L("A")),
            LI(L("B")),
            LI(L("C")),
          )),
          L("S"),
          T.UnorderedList(NEA(
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

      "codeBlocks" - {
        "flat" - test(
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
          T.CodeBlock(None, "ok\n\n\n  1\n\n\ncool"), // blank lines trimmed
          L("hello"),
          T.CodeBlock(None, "* here we go again!"), // blank lines after block removed
          L("hello again"),
          T.CodeBlock(None, " whee"), // blank lines before block removed
        )

        "inList" - test(
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
          T.UnorderedList(NEA(
            LI(T.CodeBlock(None, "ok\n\n  great")),
            LI(T.CodeBlock(None, "  hey")),
            LI(L("cool"), T.CodeBlock(None, "  good job, me")),
            LI(L("omfg"), T.CodeBlock(None, "  derp"), L("ahh")),
          )),
          L("noice")
        )

        "inList2" - test(
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
          T.UnorderedList(NEA(
            LI(L("right"), T.CodeBlock(None, "inner")),
          )),
          T.CodeBlock(None, "outer"),
          L("done")
        )

        "beforeEmptyList" - test(
          "```\nasd\n```\n* "
        )(
          T.CodeBlock(None, "asd"),
          T.UnorderedList(NEA(LI())),
        )

        "empty" - test(
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
          T.CodeBlock(None, ""),
          T.CodeBlock(None, ""),
          T.UnorderedList(NEA(
            LI(T.CodeBlock(None, "")),
            LI(L("here"), T.CodeBlock(None, ""), L("ok")),
          )),
        )

        "indentedRoot" - test(
          """
            |preventing trim
            |
            | !```  !
            | !a    !
            | ! a   !
            | !```  !
            |
            |  !```
            |  !  b
            |  !   b
            |  !```
            |
            |! ```
            |!c
            |! c
            |!  ```
            |
            |!  ```
            |!d
            |! d
            |! ```
            |
            | ! ```
            | !   e
            | !  e
            | !```
            |
            | !```
            | !   f
            | !  f
            | ! ```
            |""".stripMargin.replace("!", "")
        )(
          L("preventing trim"),
          T.CodeBlock(None, "a\n a"),
          T.CodeBlock(None, "  b\n   b"),
          T.CodeBlock(None, "c\n c"),
          T.CodeBlock(None, "d\n d"),
          T.CodeBlock(None, "   e\n  e"),
          T.CodeBlock(None, "   f\n  f"),
        )

        "weird" - test(
          "* ```\n  \u00a0\n  ```"
        )(
          T.UnorderedList(NEA(
            LI(T.CodeBlock(None, "\u00a0")),
          )),
        )

        "withLang" - test(
          """
            |``` js !
            |// here we go
            |```
            |
            |* ```tla
            |
            |  x = /\ \/ /\ \/ /\ \/ \/ /\
            |      ...
            |
            |  ```
            |""".stripMargin.replace("!", "")
        )(
          T.CodeBlock(Some("js"), "// here we go"),
          T.UnorderedList(NEA(
            LI(T.CodeBlock(Some("tla"), """x = /\ \/ /\ \/ /\ \/ """ + """\/ /\""" + "\n    ...")),
          )),
        )
      }

      "headings" - {
        import Text.{HeadingTitleFull => H}
        "simple" - test(
          """
            |# H1
            |#not
            |#  h1  again
            |## h2
            |### h3
            |#### h4
            |##### h5
            |###### h6
            |""".stripMargin.replace("!", "")
          )(
          T.Heading1(NonEmptyArraySeq(H.Literal("H1"))),
          T.Literal("#not"),
          T.Heading1(NonEmptyArraySeq(H.Literal("h1 again"))),
          T.Heading2(NonEmptyArraySeq(H.Literal("h2"))),
          T.Heading3(NonEmptyArraySeq(H.Literal("h3"))),
          T.Heading4(NonEmptyArraySeq(H.Literal("h4"))),
          T.Heading5(NonEmptyArraySeq(H.Literal("h5"))),
          T.Heading6(NonEmptyArraySeq(H.Literal("h6"))),
        )

        "space" - test(
          """
            | # H1
            | #not
            |  #   h1  again     !
            |      wow
            |
            | # # nope
            |### h3
            |
            |
            |
            |#### h4
            |
            |
            |###x
            |
            |
            |##### h5
            |x # h1
            |###### h6
            |""".stripMargin.replace("!", "")
        )(
          T.Heading1(NonEmptyArraySeq(H.Literal("H1"))),
          T.Literal("#not"),
          T.Heading1(NonEmptyArraySeq(H.Literal("h1 again"))),
          T.Literal("wow"),
          T.Heading1(NonEmptyArraySeq(H.Literal("# nope"))),
          T.Heading3(NonEmptyArraySeq(H.Literal("h3"))),
          T.Heading4(NonEmptyArraySeq(H.Literal("h4"))),
          T.Literal("###x"),
          T.Heading5(NonEmptyArraySeq(H.Literal("h5"))),
          T.Literal("x # h1"),
          T.Heading6(NonEmptyArraySeq(H.Literal("h6"))),
        )

        "combo" - test(
          """
            |# !
            |x !
            |# h1 tag: #wip   issue:   #TODO, ref: [fr1]
            |* !
            |# h1   !
            |  lit  with  space  !
            |""".stripMargin.replace("!", "")
        )(
          L("#"),
          T.blankLine,
          L("x"),
          T.Heading1(NonEmptyArraySeq(
            H.Literal("h1 tag: "),
            H.TagRef(11),
            H.Literal(" issue: "),
            H.Issue(1, I.empty),
            H.Literal(", ref: "),
            H.ReqRef(frs(1)),
          )),
          T.UnorderedList(NonEmptyArraySeq(ArraySeq.empty)),
          T.Heading1(NonEmptyArraySeq(H.Literal("h1"))),
          L("lit with space"),
        )
      }

      "useCaseStepRef" - {
        def testU(id: UseCaseStepId, stepLabel: String): Unit = {
          val stepLabelUC = wrapString(stepLabel).takeWhile(Character.isDigit).toString.toInt
          val expect = T.UseCaseStepRef(id)
          for {
            ucCtx    <- List[Option[ReqTypePos]](None, Some(1), Some(99999))
            useCtx   = ucCtx.exists(_.value ==* stepLabelUC)
            stepStr  = if (useCtx) wrapString(stepLabel).dropWhile(Character.isDigit).toString else stepLabel
            prefix   <- if (useCtx) maybeSpace else List("", "UC-", "uc", " Uc - ")
            suffix   <- maybeSpace
            dotNoise <- null :: " ." :: ". " :: "  .  " :: Nil
            chCase   <- optBool
            padZero  <- false :: true :: Nil
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

        "liveN1" - testU(11, step11_label)
        "liveN2" - testU(19, step19_label)
        "liveE1" - testU(18, step18_label)
        "liveE2" - testWithUcCtx("[E.1]", Some(1))(T.UseCaseStepRef(18))
        "deadN" - testU(16, step16_label)
        "deadN" - testU(20, step20_label)
        "deadE" - testU(17, step17_label)

        "endInX" - testLit("[1.0.X]")
        "negN1"  - testLit("[1.-1]")
        "negN2"  - testLit("[1.0.-1]")
        "negE1"  - testLit("[1.E.-1]")
        // should also test some invalid combinations
      }

      "altForms" - {
        "req" - test("[fr1][fr 1][ fr - 2 ][Mf-1 ]")(T.ReqRef(frs(1)), T.ReqRef(frs(1)), T.ReqRef(frs(2)), T.ReqRef(mfs(1)))
        "tag" - test("#wip#DEFER#V3.x")(T.TagRef(11), T.TagRef(12), T.TagRef(26))
        "issue" - test("#tbd{cool}#Todo#TBD { nice }")(
          T.Issue(2, I(I.Literal("cool"))), T.Issue(1, I.empty), T.Issue(2, I(I.Literal("nice"))))
      }

      "ambiguity" - {
        "pubid" - test("[CO1][co-1]")(T.ReqRef(cos(1)), T.ReqRef(cos(1)))
        "code"  - test("[co1][co2]")(T.CodeRef(reqCode_co1), T.CodeRef(reqCode_co2))
      }
    }

    "small" - {
      "emailAddress" - $.TextGen.emailAddress(T).mustSatisfy(propEmailAddress)
      "webAddress"   - $.TextGen.webAddress  (T).mustSatisfy(propWebAddress)
      "tex"          - $.TextGen.tex         (T).mustSatisfy(propMathTeX)
    }

    // The [parse . toString = id] property doesn't hold with dead dead/alternate CodeRefs.
    // Eg. Dead text can have CodeRefs to dead codes.
    // Parsing text only happens to live text, and it only looks at active codes.
    "big" - {
      // tester.bugHunt(0, 10000)(Prop.eval(_.all))(nyaya.test.DefaultSettings.propSettings.setSeed(0).setDebug.setSingleThreaded)
      // tester.mustSatisfyE(_.all)(nyaya.test.DefaultSettings.propSettings.setSampleSize(20000))
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
