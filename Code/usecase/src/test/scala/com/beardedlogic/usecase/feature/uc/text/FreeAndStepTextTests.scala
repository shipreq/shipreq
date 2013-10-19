package com.beardedlogic.usecase
package feature.uc.text

import org.scalatest.FunSpec
import org.scalatest.prop._
import scala.collection.immutable.TreeSet
import scalaz.syntax.apply._
import scalaz.std.list.listInstance
import db.UseCaseSummary2
import feature.uc.change._
import lib.Types._
import feature.uc.{CachedUseCaseRelations, UcParsingCtx}
import util._
import test.{TestHelpers2, TestHelpers}
import Changes._
import ParsingConfig._
import FreeTextTerms._
import reflect.ClassTag

object FreeAndStepTextTests extends TestHelpers2 {

  val UCN = (3:Short).tag[IsUseCaseNumber]
  implicit def autoCtx(sl: StepAndLabelBiMap) = UcParsingCtx(UCN, "New Third", sl, Rels)
  implicit def autoNum(i: Int) = i.toShort.tag[IsUseCaseNumber]
  implicit def autoUCId(i: Int) = i.toLong.tag[IsUseCaseIdentId]
  implicit def exTerms(t: FreeText) = t.terms

  val UCS = List(
    new UseCaseSummary2(100, 1, "First", "X"),
    new UseCaseSummary2(200, 2, "Second", "X"),
    new UseCaseSummary2(300, 3, "Old Third", "X")
  )
  val Rels = CachedUseCaseRelations(UCS)

  def filterTerms[A <: FreeTextTerm](ts: List[FreeTextTerm])(implicit m: ClassTag[A]): List[A] =
    ts.filter(a => m.runtimeClass.isAssignableFrom(a.getClass)).map(_.asInstanceOf[A])

  def oldStyleRefs(ts: List[FreeTextTerm]): Refs = filterTerms[StepRef](ts).map(t => t.id -> t.label).toMap

  def assertFlowClause(c: Option[FlowClause], refs: Refs) {
    if (refs.isEmpty)
      c should be(None)
    else
      c.get.refs should be(refs)
  }

  abstract class Tester[T <: ParsedText[T]](val parser: Parser[T]) {
    def parse(text: String) = parser.parse(text)(StepState1)
    def terms(t: T): List[FreeTextTerm]
    def assertTerms(t: T, expectedTerms: FreeTextTerm*): Unit = terms(t) shouldBe expectedTerms.toList
    def assertText(t: T, expectedText: String): Unit = t.text shouldBe expectedText
    def oldAssert(t: T, text: String, refs: Refs, refsOwnUc: Boolean): Unit = {
      assertText(t, text)
      oldStyleRefs(terms(t)) ==== refs
      filterTerms[UseCaseSelfRef](terms(t)).nonEmpty ==== refsOwnUc
    }
    def textChanged: Change
  }

  object FreeTextTester extends Tester[FreeText](FreeText) {
    override def terms(t: FreeText) = t.terms
    override def textChanged = TextChanged
  }

  object StepTextTester extends Tester[StepText](new StepTextFactory(X0)) {
    override def terms(t: StepText) = t.mainClause.terms
    override def textChanged = StepTextChanged(X0)
    override def oldAssert(subject: StepText, text: String, refs: Refs, refsOwnUc: Boolean): Unit = {
      subject.text ==== text
      oldStyleRefs(terms(subject)) ==== refs
      subject.flowFromClause shouldBe None
      subject.flowToClause shouldBe None
      FreeTextTester.oldAssert(subject.mainClause, text, refs, refsOwnUc)
    }
  }

  sealed trait FlowTester {
    type Clause <: FlowClause
    def other: FlowTester
    def get(t: StepText): Option[FlowClause]
    def change(src: LocalStepId, tgt: Set[LocalStepId]): Change
    def forceArrows(str: String): String
    def style: FlowStyle
    def obj: Flow[Clause]
  }

  object FlowFromTester extends FlowTester {
    override type Clause = FlowFromClause
    val FlowRightReplace = "(-+)>".r
    override def other = FlowToTester
    override def get(t: StepText) = t.flowFromClause
    override def change(src: LocalStepId, tgt: Set[LocalStepId]) = FlowFromChange(tgt, src)
    override def forceArrows(s: String) = FlowRightReplace.replaceAllIn(s, "<" + _.group(1)).replace("➡", "⬅")
    override def style = FlowFromStyle
    override def obj = FlowFrom
  }

  object FlowToTester extends FlowTester {
    override type Clause = FlowToClause
    val FlowLeftReplace = "<(-+)".r
    override def other = FlowFromTester
    override def get(t: StepText) = t.flowToClause
    override def change(src: LocalStepId, tgt: Set[LocalStepId]) = FlowToChange(src, tgt)
    override def forceArrows(s: String) = FlowLeftReplace.replaceAllIn(s, _.group(1) + ">").replace("⬅", "➡")
    override def style = FlowToStyle
    override def obj = FlowTo
  }

  val FlowTesters = List(FlowFromTester, FlowToTester)
}

// =====================================================================================================================

class FreeAndStepTextTests extends FunSpec with TestHelpers with PropertyChecks with Checkers {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = Cores * 40, workers = Cores)

  import FreeAndStepTextTests._

  def aTextWithRefs[T <: ParsedText[T]](T: Tester[T]) {
    implicit def StrToSomeStr(s: String) = Some(s)

    def testText(input: String, output: Option[String]): Unit = {
      val x = T.parse(input)
      x.text ==== output.getOrElse(input)
    }
    def testBoth(input: String, output: Option[String], terms: FreeTextTerm*): Unit = {
      val x = T.parse(input)
      x.text ==== output.getOrElse(input)
      T.assertTerms(x, terms: _*)
    }
    def oldTest(input: String, output: Option[String], stepRefs: Refs = Map.empty, hasUcSelfRef: Boolean = false): Unit = {
      val x = T.parse(input)
      T.oldAssert(x, output.getOrElse(input), stepRefs, hasUcSelfRef)
    }

    describe("Parsing free text") {
      it("should parse plain text") {
        testBoth("", None)
        testBoth("nothing", None, PlainText("nothing"))
      }

      it("should trim whitespace") {
        testText("  should  trim  whitespace  ", "should  trim  whitespace")
      }

      describe("Step refs") {
        it("should detect valid step refs") {
          testBoth("Umm [S.1] only", None, PlainText("Umm "), StepRef(X1, S1), PlainText(" only"))
          testBoth("Umm [S.1] & [S.3] ah and [S.1]!", None, PlainText("Umm "), StepRef(X1, S1), PlainText(" & "), StepRef(X3, S3), PlainText(" ah and "), StepRef(X1, S1), PlainText("!"))
        }

        it("should remove whitespace from step refs") {
          oldTest("[ S.1]", "[S.1]", Map(X1 -> S1))
          oldTest("[S.1 ]", "[S.1]", Map(X1 -> S1))
          oldTest("[ S.1 ]", "[S.1]", Map(X1 -> S1))
          oldTest("[S .1]", "[S.1]", Map(X1 -> S1))
          oldTest("[S. 1]", "[S.1]", Map(X1 -> S1))
          oldTest("[S . 1]", "[S.1]", Map(X1 -> S1))
          oldTest("This is [S . 1] and [ S.1 ] together!", "This is [S.1] and [S.1] together!", Map(X1 -> S1))
        }

        it("should add ? to invalid step refs") {
          oldTest("[1.0.9] doesn't exist.", "[1.0.9?] doesn't exist.")
        }

        it("should ignore existing invalid step refs") {
          oldTest("[1.0.9?] doesn't exist.", None)
        }

        it("should ignore invalid refs without dots") {
          oldTest("[123]", None)
        }

        it("should reparsed deleted refs as deleted refs") {
          testBoth(DeletedRefStr, None, DeletedRef)
          testBoth(s"Hehe $DeletedRefStr", None, PlainText("Hehe "), DeletedRef)
          testBoth(s"$DeletedRefStr no", None, DeletedRef, PlainText(" no"))
          testBoth(s"blah${DeletedRefStr}no", None, PlainText("blah"), DeletedRef, PlainText("no"))
        }
      }

      describe("Use Case refs") {
        it("should parse valid UC refs (numbers only)") {
          oldTest("[UC 1]", "[UC-1: First]")
          oldTest("[UC - 1]", "[UC-1: First]")
          oldTest("[UC- 1]", "[UC-1: First]")
          oldTest("[UC -1]", "[UC-1: First]")
          oldTest("[UC-1]", "[UC-1: First]")
          oldTest("[ UC-1 ]", "[UC-1: First]")
          oldTest("[ UC  1 ]", "[UC-1: First]")
          oldTest("[UC2]", "[UC-2: Second]")
        }
        it("should parse valid UC refs (with title)") {
          oldTest("[UC-1: Bullshit]", "[UC-1: First]")
          oldTest("[UC-1 : Bullshit ]", "[UC-1: First]")
          oldTest("[ UC 2: Blah blah blah] and [UC-1:FFS]", "[UC-2: Second] and [UC-1: First]")
        }
        it("should use the current title when referencing current use case") {
          oldTest("[UC-3]", "[UC-3: New Third]", Map.empty, true)
          oldTest("[UC-1][UC-3]", "[UC-1: First][UC-3: New Third]", Map.empty, true)
        }
      }
    } // end parsing

    describe("Loading text") {
      it("should load simple text") {
        val x = T.parser.load("Hehe".tag[IsNormalised])(BiMap.empty, StepState1)
        T.oldAssert(x, "Hehe", Map.empty, false)
      }

      it("should denormalise step refs") {
        val x = T.parser.load("Hehe [D.100]".tag[IsNormalised])(savedSteps(100 -> X2), StepState1)
        T.oldAssert(x, "Hehe [S.2]", Map(X2 -> S2), false)
      }

      it("should denormalise UC refs") {
        val x = T.parser.load("Hehe [UC-2] and [UC-1]!".tag[IsNormalised])(BiMap.empty, StepState1)
        T.oldAssert(x, "Hehe [UC-2: Second] and [UC-1: First]!", Map.empty, false)
      }

      it("should denormalise UC self-refs") {
        val x = T.parser.load("Hehe [UC-3]!".tag[IsNormalised])(BiMap.empty, UcParsingCtx(UCN, "Old Third", StepState1, Rels))
        T.oldAssert(x, "Hehe [UC-3: Old Third]!", Map.empty, true)
      }
    }

    describe("Normalising text") {
      it("should normalise UC refs to [UC-n]") {
        val txt = "Hehe [UC-2: Second] and [UC-1: First]!"
        val nt = T.parse(txt).normalisedText(BiMap.empty)
        nt shouldBe "Hehe [UC-2] and [UC-1]!"
      }
    }

    describe("Responding to a ExistingStepLabelsChanged") {
      it("should do nothing if text has no refs") {
        val x = T.parse("hehe")
        x.respondToChange(MockExistingStepLabelsChanged)(StepState2) should be(NoChange)
      }

      def test(before: String)(textAfter: String, refsAfter: Refs) {
        val x = T.parse(before)
        val y = x.respondToChange(MockExistingStepLabelsChanged)(StepState2)
        T.oldAssert(y.getValueOrElse(x), textAfter, refsAfter, false)
      }

      it("should update refs") {
        test("Umm [S.1] & [S.2] ah [S.5] and [S.1]!")("Umm [S.A] & [S.2] ah [S.F] and [S.A]!", Map(X1 -> SA, X2 -> S2, X5 -> SF))
      }

      it("should invalidate deleted refs") {
        test("Watch [S.3] go.")("Watch [DELETED] go.", Map.empty)
      }

      it("should transform as per examples") {
        val examples = Table(("Before", "After")
          , ("[S.1]", "[S.A]")
          , ("w[S.1]", "w[S.A]")
          , ("w[S.1]w", "w[S.A]w")
          , ("[S.1]w", "[S.A]w")
          , ("[S.1][S.1]", "[S.A][S.A]")
          , ("[S.1] and [S.5] and [S.5] and [S.1]", "[S.A] and [S.F] and [S.F] and [S.A]")
          , ("[S.1] hehe [S.1]", "[S.A] hehe [S.A]")
          , ("[S.2]", "[S.2]")
          , ("Whatever", "Whatever")
          , ("And S.1 is blah", "And S.1 is blah")
          , ("So [S.1] changes, [S.3] and [S.3] are gone, [S.2] is the same.", "So [S.A] changes, [DELETED] and [DELETED] are gone, [S.2] is the same.")
          , ("So [S.1 ] changes, [  S.3  ] and [S. 3  ] are gone, [  S.2] is the same.", "So [S.A] changes, [DELETED] and [DELETED] are gone, [S.2] is the same.")
        )
        forAll(examples)((before: String, after: String) => {
          val x = T.parse(before)
          val y = x.respondToChange(MockExistingStepLabelsChanged)(StepState2).getValueOrElse(x)
          y.text should be(after)
        })
      }
    }

    describe("Responding to a TitleChanged") {
      it("should change UC refs to self") {
        val x = T.parse("Look [UC-3].")
        T.oldAssert(x, "Look [UC-3: New Third].", Map.empty, true)
        val (y, changes) = x.respondToChange(TitleChanged("New Third", "GREAT"))(autoCtx(StepState1).copy(title = "GREAT")).openChange
        y.text ==== "Look [UC-3: GREAT]."
        changes shouldBe List(T.textChanged)
      }
    }
  } // end aTextWithRefs

  describe("FreeText") {
    it should behave like aTextWithRefs(FreeTextTester)
  }

  describe("StepText") {
    it should behave like aTextWithRefs(StepTextTester)

    def parse(input: String, stepState: StepAndLabelBiMap = StepState1) = new StepTextFactory(X0).parse(input)(stepState)

    def testTextToText(input: String, output: String, stepState: StepAndLabelBiMap = StepState1) = {
      val x = parse(input, stepState)
      x.text should be(output)
      x
    }

    describe("respondToChange producing StepTextChanged") {

      it("should produce one when main clause is affected") {
        val a = parse("Look at [S.1]")
        a.respondToChange(MockExistingStepLabelsChanged)(StepState2) match {
          case Changed(b, changes) =>
            b should be(StepText(X0, FreeText(PlainText("Look at ") :: StepRef(X1, SA) :: Nil), None, None))
            changes.list should contain(StepTextChanged(X0))
          case x => fail(s"Change expected, got: $x")
        }
      }

      it("should produce one when flow is affected") {
        parse("Hehe").respondToChange(FlowToChange(X5, Set(X0)))(StepState1) match {
          case Changed(b, changes) => changes.list should contain(StepTextChanged(X0))
          case x => fail(s"Change expected, got: $x")
        }
      }

      it("should not produce one when nothing changes") {
        val a = parse("Look at [S.1]")
        a.respondToChange(MockExistingStepLabelsChanged)(StepState1) match {
          case NoChange =>
          case x => fail(s"NoChange expected, got: $x")
        }
      }
    }

    // -----------------------------------------------------------------------------------------------------------------
    def aFlowClause(F: FlowTester) {

      def parsef(input: String) = parse(F.forceArrows(input))

      /** Assert text, assert subject flow, assert other flow is empty */
      def assert(x: StepText, text: String, refs: Refs) = {
        x.text should be(F.forceArrows(text))
        assertFlowClause(F.get(x), refs)
        F.other.get(x) should be(None)
        x
      }

      def testTextToTextf(input: String, output: String, stepState: StepAndLabelBiMap = StepState1) =
        testTextToText(F.forceArrows(input), F.forceArrows(output), stepState)

      /** Parse text, assert text, assert no flows */
      def testTextToTextAndNoFlows(input: String, output: String, stepState: StepAndLabelBiMap = StepState1) =
        assert(parsef(input), output, Map.empty)

      /** Combines flow text with different arrows and main clauses. Asserts text, subject flow, & other flow is empty. */
      def testFlowText(flowClauseText: String, expFlowClauseText: String, refLabels: Traversable[StepLabel]) = {
        for ((m1, m2) <- List(("", ""), ("hehe", "hehe "), ("hehe ", "hehe "))) {
          for (a <- List("<--", "<---", "⬅", "⬅ ")) {
            for (aw <- List("", " ", "    ")) {
              val input = m1 + a + aw + flowClauseText
              val output = m2 + F.style.arrow + " " + expFlowClauseText
              val x = parsef(input)
              assert(x, output, mapFromIds(refLabels))
            }
          }
        }
      }

      it("should sort refs with parents preceding children") {
        val orderedLabels = List("1.0", "1.0.1", "1.0.1.a", "1.0.1.a.2", "1.0.1.c", "1.0.2", "1.1", "1.E.1")
        val refs: Refs = orderedLabels.map(x => (s"id/$x".asLocalStepId, x.asLabel)).toMap
        val flow = F.obj.create(refs).get
        F.obj.toText(flow).replaceAll("[^0-9a-zE\\. ]", "").trim should be(orderedLabels.mkString(" "))
      }

      it("should parse a single valid ref") {
        val examples = Table(("BEFORE", "TEXT AFTER")
          , ("--> S.1", "➡ [S.1]")
          , ("--> [S.1]", "➡ [S.1]")
          , ("  -->   S.1  ", "➡ [S.1]")
          , ("great --> S.1", "great ➡ [S.1]")
          , ("great-->S.1", "great ➡ [S.1]")
          , ("-->S.1", "➡ [S.1]")
          , ("-->[S.1]", "➡ [S.1]")
        )
        forAll(examples)((before, after) => assert(parsef(before), after, Map(X1 -> S1)))
      }

      it("should parse a multiple valid refs") {
        val examples = Table(("BEFORE", "TEXT AFTER", "REFS AFTER")
          , ("S.1,S.1", "[S.1]", Set(S1))
          , ("S.1, S.2", "[S.1] [S.2]", Set(S1, S2))
          , ("S.1,S.2", "[S.1] [S.2]", Set(S1, S2))
          , ("S.1, [S.2]", "[S.1] [S.2]", Set(S1, S2))
          , ("[S.1] [S.2]", "[S.1] [S.2]", Set(S1, S2))
          , ("S.2, S.1", "[S.1] [S.2]", Set(S1, S2))
          , ("S.2, S.1, S.1", "[S.1] [S.2]", Set(S1, S2))
          , ("S.1, S.3, S.1", "[S.1] [S.3]", Set(S1, S3))
        )
        forAll(examples)(testFlowText)
      }

      it("should allow links to self") {
        testFlowText("S.0", "[S.0]", Seq(S0))
      }

      it("should resolve duplicate refs") {
        testFlowText("S.1 [S.1] S.2 [S.1]", "[S.1] [S.2]", Seq(S1, S2))
      }

      it("should handle invalid flow clauses") {
        val examples = Table(("BEFORE", "AFTER")
          , ("--> blah", "-> blah")
          , ("blarr --> blah", "blarr -> blah")
          , ("blarr -->", "blarr ->")
          , ("zxc-->abc", "zxc->abc")
          , ("zxc----->abc", "zxc->abc")
          , ("--> [S.123]", "-> [S.123?]")
          , ("--> S.123", "-> [S.123?]")
          , ("--> S.456, S.123", "-> [S.456?] [S.123?]")
          , ("--> [S.456], [S.123]", "-> [S.456?] [S.123?]")
        )
        forAll(examples)((before, after) => {
          testTextToTextf(before, after)
          testTextToTextf(before.replaceAll("-{2,}>", "➡"), after)
        })
      }

      // Examples:
      // ⬅[S.1] [E.901]               becomes <- [E.901?] ⬅ [S.1]
      // ⬅[E.901] [S.1]⬅[S.2] [E.902] becomes <- [E.901?] <- [E.902?] ⬅ [S.1] [S.2]
      it("should seperate good and bad refs in flow clauses") {
        sealed trait Ref {
          def t1: String
          def t2: String
          def good: Boolean
          def bad = !good
        }
        case object GoodRef extends Ref {
          override def t1 = "S.1"
          override def t2 = "S.2"
          override def good = true
        }
        case object BadRef extends Ref {
          override def t1 = "E.901"
          override def t2 = "E.902"
          override def good = false
        }
        case class Example(input: String, output: String) {
          def ioPair = (input, output)
        }

        def makeExample(a: List[Ref], b: List[Ref]): Example = {
          def genInp(l: List[Ref], f: Ref => String) = l match {
            case Nil => ""
            case _ => "⬅" + l.map(r => makeStepRef(f(r).tag[IsStepLabel])).mkString("")
          }
          val input = genInp(a, _.t1) + genInp(b, _.t2)

          def expBad(l: List[Ref], f: Ref => String): Option[String] = l.filter(_.bad) match {
            case Nil => None
            case ll => Some("<- " + ll.map(r => makeInvalidStepRef(f(r))).mkString(" "))
          }
          def expGood1(l: List[Ref], f: Ref => String) = l.filter(_.good).map(r => makeStepRef(f(r).tag[IsStepLabel]))
          def expGood: Option[String] = (expGood1(a, _.t1) ::: expGood1(b, _.t2)) match {
            case Nil => None
            case l => Some("⬅ " + l.mkString(" "))
          }
          val exps = expBad(a, _.t1) :: expBad(b, _.t2) :: expGood :: Nil
          val output = exps.filter(_.isDefined).map(_.get).mkString(" ")

          Example(input, output)
        }

        // Build all combinations of good & bad refs
        def examples = {
          val l = List(GoodRef, BadRef)
          val c = (1 to l.size).map(n => l.combinations(n).toList).flatten.map(_.permutations.toList).flatten.toList
          ^(c, List(Nil) ::: c)(makeExample)
        }
        //examples foreach println

        // Test
        val table = Table(("IN","OUT"),examples.map(_.ioPair): _*)
        forAll(table)(testTextToTextf(_,_))
      }

      it("should parse correct flow clauses left of erroneous flow clauses") {
        val examples = Table(("BEFORE", "AFTER")
          , ("--> S.1 --> [S.99?]", "-> [S.99?] ➡ [S.1]")
          , ("bullshit --> bullshit --> S.1 --> [S.99?]", "bullshit -> bullshit -> [S.99?] ➡ [S.1]")
        )
        forAll(examples)(testTextToTextf(_,_))
      }

      describe("Responding to a MockExistingStepLabelsChanged") {
        def test(textBefore: String, textAfter: String, refsAfter: Set[LocalStepId]) {
          val x = parse(F.forceArrows(textBefore))
          val y = x.respondToChange(MockExistingStepLabelsChanged)(StepState2).getValueOrElse(x)
          F.get(x) should not be (None)
          y.text should be(F.forceArrows(textAfter))
          assertFlowClause(F.get(y), mapToLabels(refsAfter, StepState2))
        }

        it("should update refs") {
          test("➡ [S.2] [S.5]", "➡ [S.2] [S.F]", Set(X2, X5))
        }
        it("should reorder when updating") {
          test("➡ [S.1] [S.2]" ,"➡ [S.2] [S.A]", Set(X2,X1))
        }
        it("should remove one") {
          test("➡ [S.2] [S.3]" ,"➡ [S.2]", Set(X2))
          test("➡ [S.3] [S.6]" ,"➡ [S.6]", Set(X6))
        }
        it("should remove only") {
          test("➡ [S.3]" ,"", Set())
        }
        it("should ignore non-changing") {
          test("➡ [S.2]" ,"➡ [S.2]", Set(X2))
        }
      }

      describe("Updating flow text") {
        def test(textBefore: String, refsBefore: Set[StepLabel])(newText: String, expectedToIds: Option[Set[LocalStepId]]) {
          val x = parse(F.forceArrows(textBefore))
          val cr = x.update(F.forceArrows(newText))(StepState1)
          assertFlowClause(F.get(x), mapFromIds(refsBefore))
          var expectedChanges: List[Change] = StepTextChanged(x.stepId) :: Nil
            if (expectedToIds.nonEmpty) expectedChanges :+= F.change(X0, expectedToIds.get)
          cr.getChanges should be(expectedChanges)
        }

        it("should produce a flow-change when steps first added") {
          test("", Set.empty)("--> S.1, S.1, S.2", Some(Set(X1, X2)))
        }
        it("should produce a flow-change when more steps added") {
          test("--> S.1, S.1, S.2", Set(S1, S2))("--> S.1, S.2, S.3", Some(Set(X1, X2, X3)))
        }
        it("should produce a flow-change when a step removed") {
          test("--> S.1, S.1, S.2", Set(S1, S2))("--> S.2", Some(Set(X2)))
        }
        it("should produce a flow-change when all steps removed") {
          test("--> S.1, S.1, S.2", Set(S1, S2))("blah", Some(Set()))
        }
        it("should produce a flow-change when all steps invalidated") {
          test("--> S.1, S.1, S.2", Set(S1, S2))("--> S.987 S.456", Some(Set()))
        }
        it("should produce a flow-change when some steps invalidated") {
          test("--> S.1, S.1, S.2", Set(S1, S2))("--> S.1 S.456", Some(Set(X1)))
        }
        it("should not produce a flow-change when no changes and has steps") {
          test("blah --> S.1", Set(S1))("blah and stuff --> S.1", None)
          test("blah --> S.1", Set(S1))("blah and stuff --> [S.1] [S.1]", None)
        }
        it("should not produce a flow-change when no changes and no steps") {
          test("blah", Set.empty)("blah and stuff", None)
        }
      }
    } // end aFlowClause
    // -----------------------------------------------------------------------------------------------------------------

    describe("Parsing flow-from clause") {
      it should behave like (aFlowClause(FlowFromTester))
    }
    describe("Parsing flow-to clause") {
      it should behave like (aFlowClause(FlowToTester))
    }

    describe("Parsing text with flow") {
      it("should parse both flows") {
        val x = new StepTextFactory(X0).parse("He [S.3] he ⬅ [S.2] ➡ [S.1]")(StepState1)
        x.text should be("He [S.3] he ⬅ [S.2] ➡ [S.1]")
        oldStyleRefs(x.mainClause) shouldBe Map("X3" -> "S.3")
        assertFlowClause(x.flowFromClause, Map(X2 -> S2))
        assertFlowClause(x.flowToClause, Map(X1 -> S1))
      }

      it("should parse TextWithFlowExamples") {
        forAll(TextWithFlowExamples)((input, expText, expRefsFrom, expRefsTo) => {
          val x = new StepTextFactory(X0).parse(input)(StepStateB)
          x.mainClause.text should be(expText.replaceAll("-->", FlowToStyle.arrowBadReplacement))
          assertFlowClause(x.flowFromClause, mapFromIds(expRefsFrom.asLabelC, StepStateB))
          assertFlowClause(x.flowToClause, mapFromIds(expRefsTo.asLabelC, StepStateB))
        })
      }

      it("should transform invalid flow so that the result is unambiguous") {
        val examples = Table(("BEFORE", "AFTER")
          , ("zxc➡-abc", "zxc->-abc")
          , ("zxc-➡abc", "zxc->abc")
          , ("zxc-➡-abc", "zxc->-abc")
          , ("zxc⬅-abc", "zxc<-abc")
          , ("zxc-⬅abc", "zxc-<-abc")
          , ("zxc-⬅-abc", "zxc-<-abc")
        )
        forAll(examples)(testTextToText(_, _))
      }

      it("should parse random valid statements") {
        check(HalfAssedOldProperties.validStatementProp)
      }
    }

    describe("text()") {
      it("should combine clauses") {
        implicit val ctx = UcParsingCtx.Empty.copy(stepsAndLabels = StepState1)
        val X1_S1 = Some(Map(X1 -> S1))
        val examples: TableFor4[String, Option[Refs], Option[Refs], String] = Table(("MAIN", "FROM", "TO", "EXPECTED")
          , ("", None, None, "")
          , ("hehe", None, None, "hehe")
          , ("hehe", X1_S1, None, "hehe ⬅ [S.1]")
          , ("hehe", None, X1_S1, "hehe ➡ [S.1]")
          , ("hehe", X1_S1, X1_S1, "hehe ⬅ [S.1] ➡ [S.1]")
          , ("", X1_S1, None, "⬅ [S.1]")
          , ("", None, X1_S1, "➡ [S.1]")
          , ("", X1_S1, X1_S1, "⬅ [S.1] ➡ [S.1]")
        )
        forAll(examples)((main: String, from: Option[Refs], to: Option[Refs], expected: String) => {
          val x = StepText(X0, FreeText.parse(main), from.map(FlowFromClause), to.map(FlowToClause))
          x.text should be(expected)
        })
      }
    }

    describe("Loading text with flow") {
      it("should normalise refs in all clauses") {
        val save = savedSteps(100 -> X2, 104 -> X1, 108 -> X3)
        val x = new StepTextFactory(X0).load("He [D.108] he ⬅ [D.100] ➡ [D.104]".tag[IsNormalised])(save, StepState1)
        x.text should be("He [S.3] he ⬅ [S.2] ➡ [S.1]")
        oldStyleRefs(x.mainClause) shouldBe Map("X3" -> "S.3")
        x.flowFromClause should be(Some(FlowFromClause(Map(X2 -> S2))))
        x.flowToClause should be(Some(FlowToClause(Map(X1 -> S1))))
      }
    }

    describe("Responding to FlowChangeMsgs") {
      // In these examples, a flow-change created by X1(S1) is received by step X0.
      // ToMe means that X1 has been pointed at X0.
      // ToNone means that X1 does not point at X0.
      val ToMe = Set(X0)
      val ToNone = Set.empty[LocalStepId]
      val examples: TableFor4[String, Set[LocalStepId], String, Set[LocalStepId]] =
        Table(("X0 TEXT BEFORE", "MSG FLOW TARGETS", "X0 TEXT AFTER", "X0 FLOW REFS AFTER")
          , ("hehe", ToMe, "hehe ⬅ [S.1]", Set(X1)) // add first
          , ("hehe ⬅ [S.2]", ToMe, "hehe ⬅ [S.1] [S.2]", Set(X1, X2)) // append
          , ("hehe ⬅ [S.1]", ToNone, "hehe", Set()) // remove only
          , ("hehe ⬅ [S.1] [S.2]", ToNone, "hehe ⬅ [S.2]", Set(X2)) // remove some
          , ("hehe ⬅ [S.2]", ToNone, "hehe ⬅ [S.2]", Set(X2)) // ignore
        )

      def testFlowMsgProcessing(F: FlowTester
        , textBefore: String, flowTargets: Set[LocalStepId], textAfter: String, refsAfter: Set[LocalStepId]) {
        def test(flowTargets: Set[LocalStepId]) {
          val change = F.other.change(X1, flowTargets)
          val x = parse(F.forceArrows(textBefore))
          val y = x.respondToChange(change)(StepState1).getValueOrElse(x)
          y.text should be(F.forceArrows(textAfter))
          if (refsAfter.isEmpty)
            F.get(y) should be(None)
          else
            F.get(y).map(_.refs) should be(Some(mapToLabels(refsAfter)))
          F.other.get(y) should be(F.other.get(x))
        }
        test(flowTargets)
        test(flowTargets ++ Set(X2, X3, X5, X6))
      }

      def testFlowMsgProcessingFrom(textBefore: String, flowTargets: Set[LocalStepId], textAfter: String, refsAfter: Set[LocalStepId]) =
        testFlowMsgProcessing(FlowFromTester, textBefore, flowTargets, textAfter, refsAfter)

      def testFlowMsgProcessingTo(textBefore: String, flowTargets: Set[LocalStepId], textAfter: String, refsAfter: Set[LocalStepId]) =
        testFlowMsgProcessing(FlowToTester, textBefore, flowTargets, textAfter, refsAfter)

      it(s"update flow-from clause when $FlowToChange received") {
        forAll(examples)(testFlowMsgProcessingFrom)
      }

      it(s"update flow-to clause when $FlowFromChange received") {
        forAll(examples)(testFlowMsgProcessingTo)
      }

      it("should not mirror links to self") {
        val txt = "➡ [S.1]"
        for (f <- FlowTesters) {
          val x = parse(f.forceArrows(txt))
          val change = f.change(X0, Set(X0))
          x.respondToChange(change)(StepState1) should be(NoChange)
        }
      }
    }
  } // end describe StepText
}

// =====================================================================================================================

/**
 * ScalaCheck generators and checks for SmartText.
 *
 * @since 15/05/2013
 */
object HalfAssedOldProperties {

  import org.scalacheck.Gen
  import org.scalacheck.Prop._
  import test.DataGenerators._

  // TODO It would be nice to do write proper property-checks

  val validStep = withOptionalBraces(Gen.oneOf("S.1", "S.2", "S.3", "S.5"))

  val validStatement = for {
    l <- optionalPlainText suchThat (!_.endsWith("-"))
    a <- flowToArrow
    r <- Gen.listOf1(validStep)
  } yield (l, a, r)


  val validStatementProp = forAllNoShrink(validStatement) { case (l, a, steps) if a.nonEmpty && steps.nonEmpty =>
    val t = l + a + steps.mkString(",")
    val end = if (steps.isEmpty) "" else "➡ " + TreeSet(steps: _*).map { _.replaceFirst("^\\[?", "[").replaceFirst("\\]?$", "]") }.mkString(" ")
    val exp = List(l.trim, end).filter(_.nonEmpty).mkString(" ")
    checkTextParsing(t, exp)
  }

  def checkTextParsing(text: String, expected: String) = {
    val actual = FreeAndStepTextTests.StepTextTester.parse(text).text
    (actual == expected) :|| s"Parsing failed.\nT: '$text'\nE: '$expected'\nA: '$actual'"
  }
}