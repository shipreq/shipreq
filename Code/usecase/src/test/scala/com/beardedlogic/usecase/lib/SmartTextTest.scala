package com.beardedlogic.usecase.lib

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import net.liftweb.http.CometActor
import org.scalatest.prop.{TableFor2, PropertyChecks, Checkers}
import scala.collection.immutable.TreeSet

import org.scalacheck.Prop._
import org.scalacheck.Gen
import msg.MessageCentre
import msg.Messages._

object SmartTextTest extends MockitoSugar {

  val StepState1 = Map("S.1" -> "X1", "S.2" -> "X2", "S.3" -> "X3", "S.5" -> "X5",
                        "X1" -> "S.1", "X2" -> "S.2", "X3" -> "S.3", "X5" -> "S.5")

  val StepState2 = Map("S.A" -> "X1", "S.2" -> "X2", "S.4" -> "X4", "S.F" -> "X5",
                        "X1" -> "S.A", "X2" -> "S.2", "X4" -> "S.4", "X5" -> "S.F")

  def subjectWithText(text: String) = {
    val m = new SmartText(mock[MessageCentre], () => StepState1)
    m.init
    m.text = text
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
          with ShouldMatchers
          with PropertyChecks
          with Checkers
          with SmartTextChecks
          with MockitoSugar {

  import SmartTextTest._

  class RefLookupProvider(var value: Map[String, String])

  implicit class Ext(m: SmartText) {
    def sendStepChangeMsg() {
      m.messageHandler.applyOrElse[Any, Unit](StepChangeMsg, _ => ())
    }
  }

  def any[T](implicit m: Manifest[T]) = org.mockito.Matchers.any(m.runtimeClass.asInstanceOf[Class[T]])

  override def checkTextParsing(text: String, expected: String) = {
    val actual = subjectWithText(text).text
    actual should be(expected)
    true
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe("When first created and initialised") {
    it("should register itself as a listener") {
      val msgCentre = mock[MessageCentre]
      val m = new SmartText(msgCentre, () => StepState2)
      m.init()
      verify(msgCentre).register(m)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe("text=(newText)") {
    describe("internal state") {
      it("should examine the text for step refs and create map of refs -> ids") {
        val m = new SmartText(mock[MessageCentre], () => StepState1)
        m.init
        m.text = "Umm [S.1] & [S.3] ah and [S.1]!"
        m.refsInText should be(Map("S.1" -> "X1", "S.3" -> "X3"))
      }

      it("should remove previous matches") {
        val m = new SmartText(mock[MessageCentre], () => StepState1)
        m.init
        m.refsInText = Map("S.1" -> "X1", "S.3" -> "X3")
        m.text = "Umm [S.1] only"
        m.refsInText should be(Map("S.1" -> "X1"))
      }

      it("should clear the label<->id map when no matches") {
        val m = new SmartText(mock[MessageCentre], () => StepState1)
        m.init
        m.refsInText = Map("S.1" -> "X1", "S.3" -> "X3")
        m.text = "nothing"
        m.refsInText should be('empty)
      }
    }

    // -------------------------------------------------------------------------------------------------------------------

    describe("transformations") {
      def test(input: String, expectedOutput: String = null) {
        val m = new SmartText(mock[MessageCentre], () => StepState1)
        m.init
        m.text = input
        m.text should be(if (expectedOutput == null) input else expectedOutput)
      }

      it("should add ? to invalid step refs") {
        test("[1.0.9] doesn't exist.", "[1.0.9?] doesn't exist.")
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
      test(P.FlowToRefList, examples) {
        _.replace("[", ",[").replace("]", "],").replaceAll("[\\s\\[\\]]+", "").split(",+").filter(_.nonEmpty).toList
      }
    }

    it("should parse TextAndFlowToTargets") {

      val examples = Table(("EXAMPLE", "TEXT", "REFS")
                            , ("omg --> 1.0", "omg", List("1.0"))
                            , ("hehe sweet! --> 1.3 [1.0]", "hehe sweet!", List("1.3", "1.0"))
                            , ("excellent yo --> 3.E.1,3.E.2", "excellent yo", List("3.E.1", "3.E.2"))
                            , ("excellent --> yo --> 1.0", "excellent --> yo", List("1.0"))
                          )
      forAll(examples) { (input, expText, expRefs) =>
        val r = P.parseAll(P.TextAndFlowToTargets, input)
        r.successful should be(true)
        r.get._1 should be(expText)
        r.get._2 should be(expRefs)
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

  describe("Flow-to parsing") {

    def testText(a: String, b: String) {
      subjectWithText(a).text should be(b)
      subjectWithText(a.replaceAll("-->", "→")).text should be(b)
    }

    it("should transform when invalid") {
      val examples = Table(("Before", "After")
                            , ("--> blah", "-> blah")
                            , ("blarr --> blah", "blarr -> blah")
                            , ("blarr -->", "blarr ->")
                            , ("--> [S.0]", "-> [S.0?]")
                            , ("--> S.0", "-> S.0")
                            , ("--> S.1, S.0", "-> S.1, S.0")
                            , ("--> [S.1], [S.0]", "-> [S.1], [S.0?]")
                            , ("--> S.1 bullshit", "-> S.1 bullshit")
                            , ("--> X1", "-> X1")
                          )
      forAll(examples)(testText _)
    }

    it("should parse a single valid ref") {
      val examples = Table(("Before", "After")
                            , ("--> S.1", "→ S.1")
                            , ("--> [S.1]", "→ S.1")
                            , ("  -->   S.1  ", "→ S.1")
                            , ("great --> S.1", "great → S.1")
                            , ("great-->S.1", "great → S.1")
                            , ("-->S.1", "→ S.1")
                            , ("-->[S.1]", "→ S.1")
                          )
      forAll(examples)(testText _)
    }

    it("should parse a multiple valid refs") {
      val examples = Table(("Before", "After")
                            , ("-->S.1,S.1", "→ S.1")
                            , ("--> S.1, S.2", "→ S.1, S.2")
                            , ("--> S.1,S.2", "→ S.1, S.2")
                            , ("--> S.1, [S.2]", "→ S.1, S.2")
                            , ("--> [S.1] [S.2]", "→ S.1, S.2")
                            , ("--> S.2, S.1", "→ S.1, S.2")
                            , ("--> S.2, S.1, S.1", "→ S.1, S.2")
                            , ("--> S.1, S.3, S.1", "→ S.1, S.3")
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

    it("should parse random valid statements") {
      check(validStatementProp)
    }
    it("should transform random invalid statements") {
      check(invalidStatementProp)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe("Receiving a StepChangeMsg") {

    def assertMessageDoesNothing(setup: SmartText => Unit) {
      val msgCentre = mock[MessageCentre]
      val m = new SmartText(msgCentre, () => StepState2)
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
            m.refsInText = Map("S.2" -> "X2")
            m.refAndIdLookup = StepState1
        }
      }
    }

    describe("when the ref lookup table is already up-to-date") {
      it("should do nothing") {
        assertMessageDoesNothing {
          m =>
            m.refsInText = Map("S.1" -> "X1")
            m.refAndIdLookup = StepState2
        }
      }
    }

    def newSubject(initialText: String, initialRefsInUse: Map[String, String]) = {
      val comet = mock[CometActor]
      val msgCentre = new MessageCentre(comet)
      val m = new SmartText(msgCentre, () => StepState2)
      m._text = initialText
      m.refsInText = initialRefsInUse
      m.refAndIdLookup = StepState1
      m.sendStepChangeMsg
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
        subject.refAndIdLookup should be theSameInstanceAs (StepState2)
      }
      it("should push an update") {
        verify(subject.msgCentre.cometActor).!(any[PushToClient])
      }
    }

    describe("when referenced steps change") {
      def subject = newSubject("Umm [S.1] & [S.2] ah and [S.1]!",
                                Map("S.1" -> "X1", "S.2" -> "X2"))
      it should behave like textWasUpdated(subject,
                                            "Umm [S.A] & [S.2] ah and [S.A]!",
                                            Map("S.A" -> "X1", "S.2" -> "X2"))
    }

    describe("when referenced steps are deleted") {
      def subject = newSubject("Watch [S.3] go.", Map("S.3" -> "X3"))
      it should behave like textWasUpdated(subject, "Watch [DELETED] go.", Map.empty)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  describe("Step recognition and transformation") {

    def testTransformation(before: String, expectedAfter: String) {
      val refLookupProvider = new RefLookupProvider(StepState1)
      val comet = mock[CometActor]
      val msgCentre = new MessageCentre(comet)
      val m = new SmartText(msgCentre, refLookupProvider.value _)
      m.init
      m.text = before
      m.text.replaceAll("\\s+", "") should be(before.replaceAll("\\s+", ""))
      refLookupProvider.value = StepState2
      m.sendStepChangeMsg
      m.text should be(expectedAfter)
      if (before == expectedAfter) verifyZeroInteractions(comet)
      else verify(comet).!(any[PushToClient])
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

  val text: Gen[String] = Gen.alphaStr suchThat (s => !s.contains("-->") && !s.contains("→"))
  val nothing: Gen[String] = ""
  val whitespace: Gen[String] = Gen.listOf(Gen.oneOf(' ', '\t')).map(_.mkString)
  val left: Gen[String] = text | nothing | whitespace

  val optionalWhitespace: Gen[String] = nothing | whitespace

  val arrow = for {
    w1 <- optionalWhitespace
    a <- Gen.oneOf("-->", "→")
    w2 <- optionalWhitespace
  } yield w1 + a + w2

  def optionalBraces(gen: Gen[String]): Gen[String] = for {
    wrap <- Gen.oneOf(true, false)
    label <- gen
  } yield (if (wrap) s"[$label]" else label)

  val validStep = optionalBraces(Gen.oneOf("S.1", "S.2", "S.3", "S.5"))
  val invalidStep = Gen.oneOf("S.0", ".1", "X1", "")

  val invalidStatement = for {
    l <- left
    a <- arrow
    r <- Gen.listOf(invalidStep | nothing).map(_.mkString(","))
  } yield l + a + r

  val validStatement = (for {
    l <- left
    a <- arrow
    r <- Gen.listOf1(validStep)
  } yield (l, a, r))

  val invalidStatementProp = forAll(invalidStatement) { t =>
    val exp = t.trim.replaceAll("-->|→", "->")
    checkTextParsing(t, exp)
  }

  val validStatementProp = forAll(validStatement) { x =>
    val (l, a, steps) = x
    val t = l + a + steps.mkString(",")
    val end = if (steps.isEmpty) "" else "→ " + TreeSet(steps: _*).map { _.replace("[", "").replace("]", "") }.mkString(", ")
    val exp = List(l.trim, end).filter(_.nonEmpty).mkString(" ")
    checkTextParsing(t, exp)
  }

  def checkTextParsing(text: String, expected: String) = {
    val actual = subjectWithText(text).text
    (actual == expected) :| s"'$text' should parse into '$expected', not '$actual'"
  }
}
