package com.beardedlogic.usecase
package lib

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.TreeSet

import org.mockito.Mockito._
import org.scalatest.FunSpec
import org.scalatest.mock.MockitoSugar
import org.scalatest.prop.Tables.Table
import org.scalatest.prop._

import TypeTags._
import Messages._
import test.TestHelpers
import util._

object SmartTextTest extends MockitoSugar {

  implicit def autoBiMap[A,B](m: Map[A, B]) = BiMap(m)

  val StepState1 = BiMap(
    "X1".asLocalId -> "S.1".asLabel,
    "X2".asLocalId -> "S.2".asLabel,
    "X3".asLocalId -> "S.3".asLabel,
    "X5".asLocalId -> "S.5".asLabel,
    "X6".asLocalId -> "S.6".asLabel)

  val StepState2 = BiMap(
    "X1".asLocalId -> "S.A".asLabel,
    "X2".asLocalId -> "S.2".asLabel,
    "X4".asLocalId -> "S.4".asLabel,
    "X5".asLocalId -> "S.F".asLabel,
    "X6".asLocalId -> "S.6".asLabel)

  val StepStateX2 = BiMap(
    "X1".asLocalId -> "1.0".asLabel,
    "X2".asLocalId -> "1.2".asLabel,
    "X3".asLocalId -> "1.3".asLabel,
    "X3E1".asLocalId -> "3.E.1".asLabel,
    "X3E2".asLocalId -> "3.E.2".asLabel)

  val TextWithFlowExamples = Table[String, String, List[String], List[String]](
    ("EXAMPLE", "TEXT", "REFS-FROM", "REFS-TO")
    , ("omg --> 1.0", "omg", Nil, List("1.0"))
    , ("omg <-- 1.0", "omg", List("1.0"), Nil)
    , ("omg --> 1.0 <-- 1.2", "omg", List("1.2"), List("1.0"))
    , ("omg <-- 1.0 --> 1.2", "omg", List("1.0"), List("1.2"))
    , ("hehe sweet! --> 1.3 [1.0]", "hehe sweet!", Nil, List("1.3", "1.0"))
    , ("excellent yo --> 3.E.1,3.E.2", "excellent yo", Nil, List("3.E.1", "3.E.2"))
    , ("excellent --> yo --> 1.0", "excellent --> yo", Nil, List("1.0"))
  )

  implicit class SmartTextExt(m: SmartText) {
    def text_=(txt: String) = m.setTextFromUser(txt)(NoReaction)
    def sendStepChangeMsg(implicit reactor: Reactor) {
      m.messageHandler(reactor)(StepChangeMsg)
    }
    def sendMsg(msg: Message)(implicit reactor: Reactor) {
      m.messageHandler(reactor).applyOrElse[Message, Unit](msg, _ => ())
    }
  }

  val FlowRightReplace = "(-+)>".r
  val FlowLeftReplace = "<(-+)".r

  implicit class StringFlowExt(s: String) {
    def fixArrows(from: Boolean) = if (from)
      FlowRightReplace.replaceAllIn(s, "<" + _.group(1)).replace("➡", "⬅")
    else
      FlowLeftReplace.replaceAllIn(s, _.group(1) + ">").replace("⬅", "➡")
  }

  class ReactionCollector extends Reactor {
    val reactions = new ListBuffer[Any]
    override def apply[R](t: ReactionType[R])(f: => R) {
      reactions += f
    }
  }

  class MsgCollector extends MessageCentre {
    val sent = new ListBuffer[Message]
    override def !(msg: Message)(implicit reactor: Reactor): Unit = {
      if (reactor != NoReactionOrNewMessages) sent += msg
    }
    val reactionCollector = new ReactionCollector
  }

  def textFieldWithText(text: String) = {
    val m = new SmartText(mock[MessageCentre], CachedFunction.eager0(StepState1))
    m.init
    m.setTextFromUser(text)(NoReaction)
    m
  }

  def stepFieldWithText(text: String, stepId: LocalIdStr = "SUBJ".asLocalId, refLookup: BiMap[LocalIdStr, LabelStr] = StepState1) = {
    val m = new SmartStepText(mock[MessageCentre], CachedFunction.eager0(refLookup), stepId, stepId + "-t")
    m.init
    m.setTextFromUser(text)(NoReaction)
    m
  }
}

// =====================================================================================================================

/**
 * Unit test for SmartText.
 *
 * @since 12/05/2013
 */
class SmartTextTest
  extends FunSpec
  with PropertyChecks
  with Checkers
  with SmartTextChecks
  with TestHelpers {

  import SmartTextTest._

  def assertReaction(subject: SmartText, expected: Boolean = true) {
    subject.msgCentre match {
      case m : MsgCollector => m.reactionCollector.reactions should have size(if (expected) 1 else 0)
      //case m : _ => verify(m, if (expected) times(1) else never).!(any[PushToClient])
    }
  }

  implicit def reactor = NoReaction

  // -------------------------------------------------------------------------------------------------------------------

  describe("Setting text with normalised refs") {

    it("should set simple text") {
      val m = new SmartText(mock[MessageCentre], CachedFunction.eager0(StepState1))
      m.init()
      m.refsInText += ("A".asLocalId -> "B".asLabel)
      m.setTextFromLoad("Hehe".hasNormalisedRefs, BiMap.empty)
      m.text should be("Hehe")
      m.refsInText should be('empty)
    }

    it("should set text with normalised refs") {
      val msgCentre = new MsgCollector
      val m = new SmartText(msgCentre, CachedFunction.eager0(StepState1))
      m.init()
      m.setTextFromLoad("Hehe [D.100]".hasNormalisedRefs, Map(100.tag[StepDataId] -> "X2".asLocalId))
      m.text should be("Hehe [S.2]")
      m.refsInText should be(Map("X2" -> "S.2"))
      msgCentre.sent.size should be(0)
    }

    it("should set text with normalised flow refs") {
      val msgCentre = new MsgCollector
      val m = new SmartStepText(msgCentre, CachedFunction.eager0(StepState1), "XX".asLocalId, "XXt")
      m.init()
      val savedSteps = BiMap(100.tag[StepDataId] -> "X2".asLocalId, 104.tag[StepDataId] -> "X1".asLocalId, 108.tag[StepDataId] -> "X3".asLocalId)
      val ntext = "He [D.108] he ⬅ [D.100] ➡ [D.104]".hasNormalisedRefs
      m.setTextFromLoad(ntext, savedSteps)
      m.text should be("He [S.3] he ⬅ [S.2] ➡ [S.1]")
      m.refsInText should be(Map("X3" -> "S.3"))
      m.flowFrom.refs should be(Map("X2" -> "S.2"))
      m.flowTo.refs should be(Map("X1" -> "S.1"))
      msgCentre.sent.size should be(0)
    }

    // TODO doesn't test parsing on text change
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe("When first created and initialised") {
    it("should register itself as a listener") {
      val msgCentre = mock[MessageCentre]
      val m = new SmartText(msgCentre, CachedFunction.eager0(StepState2))
      m.init()
      verify(msgCentre).register(m)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe("Plain text parsing") {
    describe("internal state") {
      it("should examine the text for step refs and create map of refs -> ids") {
        val m = new SmartText(mock[MessageCentre], CachedFunction.eager0(StepState1))
        m.init
        m setTextFromUser "Umm [S.1] & [S.3] ah and [S.1]!"
        m.refsInText should be(Map("X1" -> "S.1", "X3" -> "S.3"))
      }

      it("should remove previous matches") {
        val m = new SmartText(mock[MessageCentre], CachedFunction.eager0(StepState1))
        m.init
        m.refsInText = Map("X1".asLocalId -> "S.1".asLabel, "X3".asLocalId -> "S.3".asLabel)
        m setTextFromUser "Umm [S.1] only"
        m.refsInText should be(Map("X1" -> "S.1"))
      }

      it("should clear the label<->id map when no matches") {
        val m = new SmartText(mock[MessageCentre], CachedFunction.eager0(StepState1))
        m.init
        m.refsInText = Map("X1".asLocalId -> "S.1".asLabel, "X3".asLocalId -> "S.3".asLabel)
        m setTextFromUser "nothing"
        m.refsInText should be('empty)
      }
    }

    // -----------------------------------------------------------------------------------------------------------------

    describe("transformations") {
      def test(input: String, expectedOutput: String = null) {
        val m = new SmartText(mock[MessageCentre], CachedFunction.eager0(StepState1))
        m.init
        m setTextFromUser input
        m.text should be(if (expectedOutput == null) input else expectedOutput)
      }

      it("should add ? to invalid step refs") {
        test("[1.0.9] doesn't exist.", "[1.0.9?] doesn't exist.")
      }

      it("should not recognise normalised DB refs") {
        val id = tag[StepDataId](100)
        test(s"${SmartText.MakeNormalisedRef(id)} doesn't exist.", "[D.100?] doesn't exist.")
      }

      it("should ignore existing invalid step refs") {
        test("[1.0.9?] doesn't exist.")
      }

      it("should remove whitespace") {
        test("[ S.1]", "[S.1]")
        test("[S.1 ]", "[S.1]")
        test("[ S.1 ]", "[S.1]")
        test("[S .1]", "[S.1]")
        test("[S. 1]", "[S.1]")
        test("[S . 1]", "[S.1]")
      }

      it("should ignore words without dots") {
        test("[DELETED]")
        test("[123]")
      }
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe("MyLittleParser") {
    import SmartText.{MyLittleParser => P}

    it("should parse StepLabel") {
      val examples = Table(("EXAMPLE", "PASS")
                            , ("1.0", true)
                            , ("1.0.a", true)
                            , ("1.0.a.iii", true)
                            , ("1.0.a.iii.1", true)
                            , ("1.E.2", true)
                            , ("13.E.20.ba.xiv.23", true)
                            , ("13. E . 20  .ba.xiv.23", true)
                            , ("1.", false)
                            , (".0", false)
                            , ("1", false)
                            , ("1..0", false)
                            , ("X1", false)
                            , ("", false)
                          )
      test(P.StepLabel, examples)(_.replaceAll("\\s+", ""))
    }

    it("should parse OptionallyBracedRef") {
      val examples = Table(("EXAMPLE", "PASS")
                            , ("1.0", true)
                            , ("1.0.a.iii.1", true)
                            , ("1.E.2", true)
                            , ("13.E.20.ab.xiv.23", true)
                            , ("[1.0]", true)
                            , ("[13.E.20.ab.xiv.23]", true)
                            , ("[ 1.0 ]", true)
                            , ("[ 13.E.20.ab.xiv.23 ]", true)
                            , ("[1.0", false)
                            , ("1.0]", false)
                            , ("[[1.0]", false)
                            , ("[1.0]]", false)
                            , ("[[1.0]]", false)
                            , ("", false)
                          )
      test(P.OptionallyBracedRef, examples)(_.replaceAll("[\\s\\[\\]]+", ""))
    }

    it("should parse FlowToRefList") {
      val examples = Table(("EXAMPLE", "PASS")
                            , ("1.0", true)
                            , ("1.0, 1.2", true)
                            , ("[1.0], [1.2]", true)
                            , ("[1.0] [1.2]", true)
                            , ("[1.0] 1.2", true)
                            , ("1.3 [1.0]", true)
                            , ("1.0, 1.2. a, 1.3, 1.1", true)
                            , ("[1.0] 1.2. a, [1.3] [1.1]", true)
                            , ("", false)
                          )
      test(P.FlowRefList, examples) {
        _.replace("[", ",[").replace("]", "],").replaceAll("[\\s\\[\\]]+", "").split(",+").filter(_.nonEmpty).toList
      }
    }

    it("should parse TextAndFlow") {
      forAll(TextWithFlowExamples) { (input, expText, expRefsFrom, expRefsTo) =>
        val r = P.parseAll(P.TextAndFlow, input)
        r.successful should be(true)
        r.get._1 should be(expText)
        val flowResults = r.get._2
        flowResults.from should be(if (expRefsFrom.isEmpty) None else Some(expRefsFrom))
        flowResults.to should be(if (expRefsTo.isEmpty) None else Some(expRefsTo))
      }
    }

    def test[T](parser: P.Parser[T], examples: TableFor2[String, Boolean])(expect: String => T) {
      forAll(examples) { (input, pass) =>
        val r = P.parseAll(parser, input)
        r.successful should be(pass)
        if (pass) r.get should be(expect(input))
      }
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe("Flow parsing") {

    it("should only run on step fields (ie. you can't flow from steps into normal text fields like Actors)") {
      val input = "--> S.1,S.1"
      stepFieldWithText(input).text should be("➡ [S.1]")
      textFieldWithText(input).text should be(input)
    }

    it("should record flow ref IDs") {
      // Manual test
      val s2 = stepFieldWithText("manual test --> 1.0 <-- 1.2", refLookup = StepStateX2)
      s2.flowFrom.refs should be(Map("X2" -> "1.2"))
      s2.flowTo.refs should be(Map("X1" -> "1.0"))

      // Use shared examples + id lookup
      forAll(TextWithFlowExamples) { (input, expText, expRefsFrom, expRefsTo) =>
        val s = stepFieldWithText(input, refLookup = StepStateX2)
        s.flowFrom.refs should be(expRefsFrom.asLabels.map(l => (StepStateX2.ba(l), l)).toMap)
        s.flowTo.refs should be(expRefsTo.asLabels.map(l => (StepStateX2.ba(l), l)).toMap)
      }
    }

    def textTransformation(from:Boolean) {

      def testTextOneWay(a: String, b: String): Unit = stepFieldWithText(a).text should be(b)

      def testText(a: String, b: String) {
        stepFieldWithText(a.fixArrows(from)).text should be(b.fixArrows(from))
        stepFieldWithText(a.replaceAll("-{2,}>", "➡").fixArrows(from)).text should be(b.fixArrows(from))
      }

      it("should allow links to self") {
        stepFieldWithText("--> S.1".fixArrows(from), "X1".asLocalId).text should be("➡ [S.1]".fixArrows(from))
      }

      it("should transform when invalid") {
        val examples = Table(("Before", "After")
                              , ("--> blah", "-> blah")
                              , ("blarr --> blah", "blarr -> blah")
                              , ("blarr -->", "blarr ->")
                              , ("zxc-->abc", "zxc->abc")
                              , ("zxc----->abc", "zxc->abc")
                              , ("--> [S.0]", "-> [S.0?]")
                              , ("--> S.0", "-> S.0")
                              , ("--> S.1, S.0", "-> S.1, S.0")
                              , ("--> [S.1], [S.0]", "-> [S.1], [S.0?]")
                              , ("--> S.1 bullshit", "-> S.1 bullshit")
                              , ("--> X1", "-> X1")
                            )
        val examples2 = Table(("Before", "After")
          , ("zxc➡-abc", "zxc->-abc")
          , ("zxc-➡abc", "zxc->abc")
          , ("zxc-➡-abc", "zxc->-abc")
          , ("zxc⬅-abc", "zxc<-abc")
          , ("zxc-⬅abc", "zxc-<-abc")
          , ("zxc-⬅-abc", "zxc-<-abc")
        )
        forAll(examples)(testText)
        forAll(examples2)(testTextOneWay)
      }

      it("should parse a single valid ref") {
        val examples = Table(("Before", "After")
                              , ("--> S.1", "➡ [S.1]")
                              , ("--> [S.1]", "➡ [S.1]")
                              , ("  -->   S.1  ", "➡ [S.1]")
                              , ("great --> S.1", "great ➡ [S.1]")
                              , ("great-->S.1", "great ➡ [S.1]")
                              , ("-->S.1", "➡ [S.1]")
                              , ("-->[S.1]", "➡ [S.1]")
                            )
        forAll(examples)(testText _)
      }

      it("should parse a multiple valid refs") {
        val examples = Table(("Before", "After")
                              , ("-->S.1,S.1", "➡ [S.1]")
                              , ("--> S.1, S.2", "➡ [S.1] [S.2]")
                              , ("--> S.1,S.2", "➡ [S.1] [S.2]")
                              , ("--> S.1, [S.2]", "➡ [S.1] [S.2]")
                              , ("--> [S.1] [S.2]", "➡ [S.1] [S.2]")
                              , ("--> S.2, S.1", "➡ [S.1] [S.2]")
                              , ("--> S.2, S.1, S.1", "➡ [S.1] [S.2]")
                              , ("--> S.1, S.3, S.1", "➡ [S.1] [S.3]")
                            )
        forAll(examples)(testText _)
      }

      it("should transform a multiple valid & invalid refs") {
        val examples = Table(("Before", "After")
                              , ("--> [S.0], [S.1]", "-> [S.0?], [S.1]")
                              , ("--> S.1, X1", "-> S.1, X1")
                            )
        forAll(examples)(testText _)
      }
    }

    describe("text transformation: ⬅") {
      it should behave like textTransformation(true)
    }

    describe("text transformation: ➡") {
      it should behave like textTransformation(false)

      it("should parse random valid statements") {
        check(validStatementProp)
      }
      it("should transform random invalid statements") {
        check(invalidStatementProp)
      }
    }

    def flowChange(from:Boolean) = {
      def test(textBefore: String, newText: String, expectedToIds: Option[Set[String]]) {
        implicit val reactor = NoReaction
        val m = new MsgCollector
        val s = new SmartStepText(m, CachedFunction.eager0(StepState1), "SUBJ".asLocalId, "SUBJ-t")
        s.init()
        if (textBefore.nonEmpty) s setTextFromUser textBefore.fixArrows(from)
        m.sent.clear()
        s setTextFromUser newText.fixArrows(from)
        val exp = if (expectedToIds.isEmpty) {
          List.empty
        } else if (from) {
          FlowFromChangeMsg(expectedToIds.get.asLocalIds, "SUBJ".asLocalId) :: Nil
        } else {
          FlowToChangeMsg("SUBJ".asLocalId, expectedToIds.get.asLocalIds) :: Nil
        }
        m.sent should be(exp)
      }

      it("should broadcast when steps first added") {
        test("", "--> S.1, S.1, S.2", Some(Set("X1", "X2")))
      }
      it("should broadcast when more steps added") {
        test("--> S.1, S.1, S.2", "--> S.1, S.2, S.3", Some(Set("X1", "X2", "X3")))
      }
      it("should broadcast when a step removed") {
        test("--> S.1, S.1, S.2", "--> S.2", Some(Set("X2")))
      }
      it("should broadcast when all steps removed") {
        test("--> S.1, S.1, S.2", "blah", Some(Set()))
      }
      it("should not broadcast when no changes and has steps") {
        test("blah --> S.1", "blah and stuff --> S.1", None)
      }
      it("should not broadcast when no changes and no steps") {
        test("blah", "blah and stuff", None)
      }
    }

    describe(s"$FlowFromChangeMsg broadcasting") {
      it should behave like flowChange(true)
    }

    describe(s"$FlowToChangeMsg broadcasting") {
      it should behave like flowChange(false)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe(s"Receiving a $StepChangeMsg") {

    def assertMessageDoesNothing(setup: SmartText => Unit) {
      val msgCentre = mock[MessageCentre]
      val m = new SmartText(msgCentre, CachedFunction.eager0(StepState2))
      setup(m)
      m.sendStepChangeMsg
      verifyNoMoreInteractions(msgCentre)
    }

    describe("when there are no steps referenced") {
      it("should do nothing") {
        assertMessageDoesNothing(_ => ())
      }
    }

    describe("when there are steps referenced but none are affected") {
      it("should do nothing") {
        assertMessageDoesNothing {
          m =>
            m.refsInText = Map("X2".asLocalId -> "S.2".asLabel)
            m.refAndIdLookup << StepState1
        }
      }
    }

    describe("when the ref lookup table is already up-to-date") {
      it("should do nothing") {
        assertMessageDoesNothing {
          m =>
            m.refsInText = Map("X1".asLocalId -> "S.1".asLabel)
            m.refAndIdLookup << StepState2
        }
      }
    }

    def newSubject(initialText: String, initialRefsInUse: Map[LocalIdStr, LabelStr], useSmartStepText: Boolean = false) = {
      val msgCentre = new MsgCollector
      val m = if (useSmartStepText) {
        val s2 = new SmartStepText(msgCentre, CachedFunction.eager0(StepState2), "".asLocalId, "")
        s2.init
        // dont put flow stuff here
        s2.textWithoutFlow = initialText
        s2
      } else {
        val m = new SmartText(msgCentre, CachedFunction.eager0(StepState2))
        m.init
        m
      }
      m._text = initialText
      m.refsInText = initialRefsInUse
      m.refAndIdLookup << StepState1
      m.sendStepChangeMsg(msgCentre.reactionCollector)
      m
    }

    def textWasUpdated(subject: => SmartText, newText: String, newRefsInUse: Map[String, String]) {
      it("should update the text") {
        subject.text should be(newText)
      }
      it("should update the internal ref->id map") {
        subject.refsInText should be(newRefsInUse)
      }
      it("should record the last used ref lookup table") {
        subject.refAndIdLookup.get should be theSameInstanceAs (StepState2)
      }
      it("should react with JavaScript") {
        assertReaction(subject)
      }
    }

    describe("when referenced steps change (with SmartText)") {
      def subject = newSubject("Umm [S.1] & [S.2] ah and [S.1]!",
        Map("X1".asLocalId -> "S.1".asLabel, "X2".asLocalId -> "S.2".asLabel))
      it should behave like textWasUpdated(subject,
                                            "Umm [S.A] & [S.2] ah and [S.A]!",
                                            Map("X1" -> "S.A", "X2" -> "S.2"))
    }

    describe("when referenced steps change (with SmartStepText)") {
      def subject = newSubject("Umm [S.1] & [S.2] ah and [S.1]!",
        Map("X1".asLocalId -> "S.1".asLabel, "X2".asLocalId -> "S.2".asLabel), true)
      it should behave like textWasUpdated(subject,
                                            "Umm [S.A] & [S.2] ah and [S.A]!",
                                            Map("X1" -> "S.A", "X2" -> "S.2"))
    }

    describe("when referenced steps are deleted") {
      def subject = newSubject("Watch [S.3] go.", Map("X3".asLocalId -> "S.3".asLabel))
      it should behave like textWasUpdated(subject, "Watch [DELETED] go.", Map.empty)
    }

    def testSubject2(initialText: String) = {
      val msgCentre = new MsgCollector
      val s = new SmartStepText(msgCentre, CachedFunction.eager0(StepState2), "".asLocalId, "")
      s.init
      s.refAndIdLookup << StepState1
      s setTextFromUser initialText
      s.text should be (initialText)
      s.sendStepChangeMsg(msgCentre.reactionCollector)
      s
    }

    def updateFlowRefs(from: Boolean) {
      def test(_initialText: String, _expectedText: String, expectedIds: Set[String]) {
        val initialText = _initialText.fixArrows(from)
        val expectedText = _expectedText.fixArrows(from)
        val changeExpected = (initialText != expectedText)
        val s = testSubject2(initialText)
        val refs = if (from) s.flowFrom.refs else s.flowTo.refs
        if (changeExpected) s.refAndIdLookup.get should be theSameInstanceAs(StepState2)
        refs should be(expectedIds.asLocalIds.map(id => (id, StepState2.ab(id))).toMap)
        s.text should be(expectedText)
        assertReaction(s, changeExpected)
      }

      def testWithText(_initialText: String, _expectedText: String, expectedIds: Set[String]) {
        test(_initialText, _expectedText, expectedIds)
        test("cool " + _initialText, if (_expectedText.isEmpty()) "cool" else "cool " + _expectedText, expectedIds)
      }

      it("should update refs") {
        testWithText("➡ [S.2] [S.5]" ,"➡ [S.2] [S.F]", Set("X2","X5"))
      }
      it("should reorder when updating") {
        testWithText("➡ [S.1] [S.2]" ,"➡ [S.2] [S.A]", Set("X2","X1"))
      }
      it("should remove one") {
        testWithText("➡ [S.2] [S.3]" ,"➡ [S.2]", Set("X2"))
        testWithText("➡ [S.3] [S.6]" ,"➡ [S.6]", Set("X6"))
      }
      it("should remove only") {
        testWithText("➡ [S.3]" ,"", Set())
      }
      it("should ignore non-changing") {
        testWithText("➡ [S.2]" ,"➡ [S.2]", Set("X2"))
      }
      it("should ignore when unaffected") {
        test("haha","haha", Set())
      }
    }

    describe("Flow-from refs") {
      it should behave like updateFlowRefs(true)
    }
    describe("Flow-to refs") {
      it should behave like updateFlowRefs(false)
    }

    describe("Mixed clauses") {
      it("should update all clauses and broadcast once") {
        val examples = Table(("Before", "After")
                            , ("Blah [S.5]. ⬅ [S.2] ➡ [S.6]", "Blah [S.F]. ⬅ [S.2] ➡ [S.6]")
                            , ("Blah [S.5]. ⬅ [S.1] ➡ [S.5]", "Blah [S.F]. ⬅ [S.A] ➡ [S.F]")
                            , ("Blah [S.3]. ⬅ [S.1] ➡ [S.3]", "Blah [DELETED]. ⬅ [S.A]")
                            , ("Blah [S.3]. ⬅ [S.3] ➡ [S.1]", "Blah [DELETED]. ➡ [S.A]")
                            , ("Blah [S.3]. ⬅ [S.3] ➡ [S.3]", "Blah [DELETED].")
                          )
        forAll(examples){ (b,a) =>
          val s = testSubject2(b)
          s.text should be(a)
          assertReaction(s)
        }
      }
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe(s"Receiving FlowChangeMsgs") {

    def testSubject(initialText: String) = {
      val s = new SmartStepText(new MsgCollector, CachedFunction.eager0(StepState1), "SUBJ".asLocalId, "")
      s.init
      s setTextFromUser initialText
      s.text should be (initialText)
      s
    }

    val ToMe = Set("SUBJ")
    val ToNone = Set.empty[String]
    val examples = Table(("Text Before", "S.1's new Flow-To Targets", "Text After", "FlowFrom Ids After")
        , ("hehe", ToMe, "hehe ⬅ [S.1]", Set("X1")) // add first
        , ("hehe ⬅ [S.2]", ToMe, "hehe ⬅ [S.1] [S.2]", Set("X1","X2")) // append
        , ("hehe ⬅ [S.1]", ToNone, "hehe", Set()) // remove only
        , ("hehe ⬅ [S.1] [S.2]", ToNone, "hehe ⬅ [S.2]", Set("X2")) // remove some
        , ("hehe ⬅ [S.2]", ToNone, "hehe ⬅ [S.2]", Set("X2")) // ignore
        )

    it("should not mirror links to self") {
      for (txt <- List("➡ [S.1]","⬅ [S.1]")) {
        val s = new SmartStepText(new MsgCollector, CachedFunction.eager0(StepState1), "X1".asLocalId, "")
        val mc = s.msgCentre.asInstanceOf[MsgCollector]
        s.init
        s setTextFromUser txt
        mc.sent.size should be(1)
        s.sendMsg(mc.sent.head)(mc.reactionCollector)
        s.text should be (txt)
      }
    }

    it("update flow-from text") {
      forAll(examples){ (textBefore, flowToTargets, textAfter, idsAfter) =>
        def test(flowToTargets: Set[String]) {
          val s = testSubject(textBefore)
          val mc = s.msgCentre.asInstanceOf[MsgCollector]
          val flowToBefore = s.flowTo
          mc.sent.clear
          s.sendMsg(FlowToChangeMsg("X1".asLocalId, flowToTargets.asLocalIds))(mc.reactionCollector)
          s.text should be (textAfter)
          s.flowFrom.refs should be(idsAfter.asInstanceOf[Set[LocalIdStr]].map(id => (id,StepState1.ab(id))).toMap)
          s.flowTo should be(flowToBefore)
          assertReaction(s, textBefore != textAfter)
          mc.sent should be ('empty)
        }
        test(flowToTargets)
        test(flowToTargets ++ Set("X2","X3","X5","X6"))
      }
    }

    it("update flow-to text") {
      forAll(examples){ (textBefore, flowFromSources, textAfter, idsAfter) =>
        def test(flowToTargets: Set[String]) {
          val s = testSubject(textBefore.fixArrows(false))
          val mc = s.msgCentre.asInstanceOf[MsgCollector]
          val flowFromBefore = s.flowFrom
          mc.sent.clear
          s .sendMsg(FlowFromChangeMsg(flowFromSources.asLocalIds, "X1".asLocalId))(mc.reactionCollector)
          s.text should be (textAfter.fixArrows(false))
          s.flowTo.refs should be(idsAfter.asInstanceOf[Set[LocalIdStr]].map(id => (id,StepState1.ab(id))).toMap)
          s.flowFrom should be(flowFromBefore)
          assertReaction(s, textBefore != textAfter)
          mc.sent should be ('empty)
        }
        test(flowFromSources)
        test(flowFromSources ++ Set("X2","X3","X5","X6"))
      }
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe("Step recognition and transformation") {

    def testTransformation(before: String, expectedAfter: String) {
      val msgCentre = new MsgCollector
      val cfn = CachedFunction.eager0(StepState1)
      val m = new SmartText(msgCentre, cfn)
      m.init
      m setTextFromUser before
      m.text.replaceAll("\\s+", "") should be(before.replaceAll("\\s+", ""))
      cfn << StepState2
      m.sendStepChangeMsg(msgCentre.reactionCollector)
      m.text should be(expectedAfter)
      assertReaction(m, before != expectedAfter)
    }

    it("should work as per examples") {
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
      forAll(examples)(testTransformation _)
    }
  }
}

// =====================================================================================================================

/**
 * ScalaCheck generators and checks for SmartText.
 *
 * @since 15/05/2013
 */
trait SmartTextChecks {

  import SmartTextTest._
  import org.scalacheck.Prop._
  import org.scalacheck.Gen
  import test.DataGenerators._
  import SmartText._

  val validStep = withOptionalBraces(Gen.oneOf("S.1", "S.2", "S.3", "S.5"))
  val invalidStep = Gen.oneOf("S.0", ".1", "X1", "")

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
    val actual = stepFieldWithText(text).text
    (actual == expected) :|| s"Parsing failed.\nT: '$text'\nE: '$expected'\nA: '$actual'"
  }
}
