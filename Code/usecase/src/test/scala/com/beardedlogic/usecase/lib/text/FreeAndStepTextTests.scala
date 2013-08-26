package com.beardedlogic.usecase
package lib.text

import org.scalatest.FunSpec
import org.scalatest.prop._
import scala.collection.immutable.TreeSet
import lib.change._
import lib.Types._
import util._
import test.TestHelpers
import Changes._
import ParsingConfig.{FlowFromStyle, FlowStyle, FlowToStyle}

object FreeAndStepTextTests extends TestHelpers {

  def assertFlowClause(c: Option[FlowClause], refs: Refs) {
    if (refs.isEmpty)
      c should be(None)
    else
      c.get.refs should be(refs)
  }

  abstract class Tester[T <: ParsedText[T]](val parser: Parser[T]) {
    def parse(text: String) = parser.parse(text)(StepState1)
    def refs(t: T): Refs
    def assert(subject: T, text: String, refs: Refs): Unit
  }

  object FreeTextTester extends Tester[FreeText](FreeText) {
    override def refs(t: FreeText) = t.refs
    override def assert(subject: FreeText, text: String, refs: Refs) {
      subject.text should be(text)
      subject.refs should be(refs)
    }
  }

  object StepTextTester extends Tester[StepText](new StepTextFactory(X0)) {
    override def refs(t: StepText) = t.allRefs
    override def assert(subject: StepText, text: String, refs: Refs) {
      subject.text should be(text)
      subject.allRefs should be(refs)
      subject.flowFromClause should be(None)
      subject.flowToClause should be(None)
      FreeTextTester.assert(subject.mainClause, text, refs)
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

  import FreeAndStepTextTests._

  def aTextWithRefs[T <: ParsedText[T]](T: Tester[T]) {
    implicit def StrToSomeStr(s: String) = Some(s)

    def test(input: String, output: Option[String], refs: Refs) {
      val x = T.parse(input)
      T.assert(x, output.getOrElse(input), refs)
    }

    describe("Parsing free text") {
      it("should parse plain text") {
        test("", None, Map.empty)
        test("nothing", None, Map.empty)
      }

      it("should trim whitespace") {
        test("  should  trim  whitespace  ", "should  trim  whitespace", Map.empty)
      }

      it("should detect valid refs") {
        test("Umm [S.1] only", None, Map(X1 -> S1))
        test("Umm [S.1] & [S.3] ah and [S.1]!", None, Map(X1 -> S1, X3 -> S3))
      }

      it("should remove whitespace from refs") {
        test("[ S.1]", "[S.1]", Map(X1 -> S1))
        test("[S.1 ]", "[S.1]", Map(X1 -> S1))
        test("[ S.1 ]", "[S.1]", Map(X1 -> S1))
        test("[S .1]", "[S.1]", Map(X1 -> S1))
        test("[S. 1]", "[S.1]", Map(X1 -> S1))
        test("[S . 1]", "[S.1]", Map(X1 -> S1))
        test("This is [S . 1] and [ S.1 ] together!", "This is [S.1] and [S.1] together!", Map(X1 -> S1))
      }

      it("should add ? to invalid step refs") {
        test("[1.0.9] doesn't exist.", "[1.0.9?] doesn't exist.", Map.empty)
      }

      it("should ignore existing invalid step refs") {
        test("[1.0.9?] doesn't exist.", None, Map.empty)
      }

      it("should ignore invalid refs without dots") {
        test("[DELETED]", None, Map.empty)
        test("[123]", None, Map.empty)
      }
    }

    describe("Loading free text") {
      it("should load simple text") {
        val x = T.parser.load("Hehe".hasNormalisedRefs)(BiMap.empty, StepState1)
        T.assert(x, "Hehe", Map.empty)
      }

      it("should set text with normalised refs") {
        val x = T.parser.load("Hehe [D.100]".hasNormalisedRefs)(savedSteps(100 -> X2), StepState1)
        T.assert(x, "Hehe [S.2]", Map(X2 -> S2))
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
        T.assert(y.getOrElse(x), textAfter, refsAfter)
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
          val y = x.respondToChange(MockExistingStepLabelsChanged)(StepState2).getOrElse(x)
          y.text should be(after)
        })
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
            b should be(StepText(X0, FreeText("Look at [S.A]", Map(X1 -> SA)), None, None))
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
      def testFlowText(flowClauseText: String, expFlowClauseText: String, refLabels: Traversable[LabelStr]) = {
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

      it("should invalidate flow clause when valid & invalid refs found") {
        val examples = Table(("BEFORE", "AFTER")
          , ("--> [S.123], [S.1]", "-> [S.123?], [S.1]")
          , ("➡ [S.123], [S.1]", "-> [S.123?], [S.1]")
          , ("--> S.1, X1", "-> S.1, X1")
        )
        forAll(examples)(testTextToTextAndNoFlows(_, _))
      }

      it("should handle invalid flow clauses") {
        val examples = Table(("BEFORE", "AFTER")
          , ("--> blah", "-> blah")
          , ("blarr --> blah", "blarr -> blah")
          , ("blarr -->", "blarr ->")
          , ("zxc-->abc", "zxc->abc")
          , ("zxc----->abc", "zxc->abc")
          , ("--> [S.123]", "-> [S.123?]")
          , ("--> S.123", "-> S.123")
          , ("--> S.1, S.123", "-> S.1, S.123")
          , ("--> [S.1], [S.123]", "-> [S.1], [S.123?]")
          , ("--> S.1 bullshit", "-> S.1 bullshit")
          , ("--> X1", "-> X1")
        )
        forAll(examples)((before, after) => {
          testTextToTextf(before, after)
          testTextToTextf(before.replaceAll("-{2,}>", "➡"), after)
        })
      }

      describe(s"Responding to a $MockExistingStepLabelsChanged") {
        def test(textBefore: String, textAfter: String, refsAfter: Set[LocalStepId]) {
          val x = parse(F.forceArrows(textBefore))
          val y = x.respondToChange(MockExistingStepLabelsChanged)(StepState2).getOrElse(x)
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
        def test(textBefore: String, refsBefore: Set[LabelStr])(newText: String, expectedToIds: Option[Set[LocalStepId]]) {
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
        it("should not produce a flow-change when no changes and has steps") {
          test("blah --> S.1", Set(S1))("blah and stuff --> S.1", None)
          test("blah --> S.1", Set(S1))("blah and stuff --> [S.1] [S.1]", None)
        }
        it("should not produce a flow-change when no changes and no steps") {
          test("blah", Set.empty)("blah and stuff", None)
        }
      }
    } // end aFlowClause

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
        x.mainClause.refs should be(Map("X3" -> "S.3"))
        assertFlowClause(x.flowFromClause, Map(X2 -> S2))
        assertFlowClause(x.flowToClause, Map(X1 -> S1))
      }

      it("should parse TextWithFlowExamples") {
        forAll(TextWithFlowExamples)((input, expText, expRefsFrom, expRefsTo) => {
          val x = new StepTextFactory(X0).parse(input)(StepStateB)
          x.mainClause.text should be(expText.replaceAll("-->", FlowToStyle.arrowBadReplacement))
          assertFlowClause(x.flowFromClause, mapFromIds(expRefsFrom.asLabels, StepStateB))
          assertFlowClause(x.flowToClause, mapFromIds(expRefsTo.asLabels, StepStateB))
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
      it("should transform random invalid statements") {
        check(HalfAssedOldProperties.invalidStatementProp)
      }
    }

    describe("Text") {
      it("should combine clauses") {
        implicit val ss = StepState1
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
        val x = new StepTextFactory(X0).load("He [D.108] he ⬅ [D.100] ➡ [D.104]".hasNormalisedRefs)(save, StepState1)
        x.text should be("He [S.3] he ⬅ [S.2] ➡ [S.1]")
        x.mainClause.refs should be(Map("X3" -> "S.3"))
        x.flowFromClause should be(Some(FlowFromClause(Map(X2 -> S2))))
        x.flowToClause should be(Some(FlowToClause(Map(X1 -> S1))))
      }
    }

    describe(s"Responding to FlowChangeMsgs") {
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
          val y = x.respondToChange(change)(StepState1).getOrElse(x)
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
  val invalidStep = Gen.oneOf("S.123", ".1", "X1", "")

  val invalidStatement = for {
    l <- optionalPlainText suchThat (!_.endsWith("-"))
    a <- flowToArrow
    r <- Gen.listOf(invalidStep | nothing).map(_.mkString(","))
  } yield l + a + r

  val validStatement = for {
    l <- optionalPlainText suchThat (!_.endsWith("-"))
    a <- flowToArrow
    r <- Gen.listOf1(validStep)
  } yield (l, a, r)

  val invalidStatementProp = forAllNoShrink(invalidStatement) { t =>
    val exp = FlowToStyle.replaceAllArrowsWithBad(t.trim)
    checkTextParsing(t, exp)
  }

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