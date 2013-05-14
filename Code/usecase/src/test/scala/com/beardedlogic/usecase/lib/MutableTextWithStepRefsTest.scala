package com.beardedlogic.usecase.lib

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.beardedlogic.usecase.lib.msg.{PushToClient, MessageCentre}
import org.scalatest.mock.MockitoSugar
import com.beardedlogic.usecase.lib.field.CourseFields.StepChangeMsg
import org.mockito.Mockito._
import net.liftweb.http.CometActor
import org.scalatest.prop.TableDrivenPropertyChecks

class MutableTextWithStepRefsTest
  extends FunSpec
          with ShouldMatchers
          with TableDrivenPropertyChecks
          with MockitoSugar {

  val StepState1 = Map("S.1" -> "X1", "S.2" -> "X2", "S.3" -> "X3", "S.5" -> "X5",
                        "X1" -> "S.1", "X2" -> "S.2", "X3" -> "S.3", "X5" -> "S.5")

  val StepState2 = Map("S.A" -> "X1", "S.2" -> "X2", "S.4" -> "X4", "S.F" -> "X5",
                        "X1" -> "S.A", "X2" -> "S.2", "X4" -> "S.4", "X5" -> "S.F")

  class RefLookupProvider(var value: Map[String, String])

  implicit class Ext(m: MutableTextWithStepRefs) {
    def sendStepChangeMsg() {
      m.messageHandler.applyOrElse[Any, Unit](StepChangeMsg, _ => ())
    }
  }

  def any[T](implicit m: Manifest[T]) = org.mockito.Matchers.any(m.runtimeClass.asInstanceOf[Class[T]])

  describe("When first created and initialised") {
    it("should register itself as a listener") {
      val msgCentre = mock[MessageCentre]
      val m = new MutableTextWithStepRefs(msgCentre, () => StepState2)
      m.init()
      verify(msgCentre).register(m)
    }
  }

  describe("text=(newText)") {
    describe("internal state") {
      it("should record the label<->id map used to check for refs") {
        val m = new MutableTextWithStepRefs(null, () => StepState1)
        m.curRefLookup = StepState2
        m.text = "x"
        m.curRefLookup should be theSameInstanceAs (StepState1)
      }

      it("should examine the text for step refs and create map of refs -> ids") {
        val m = new MutableTextWithStepRefs(null, () => StepState1)
        m.text = "Umm [S.1] & [S.3] ah and [S.1]!"
        m.curRefsInUse should be(Map("S.1" -> "X1", "S.3" -> "X3"))
      }

      it("should remove previous matches") {
        val m = new MutableTextWithStepRefs(null, () => StepState1)
        m.curRefsInUse = Map("S.1" -> "X1", "S.3" -> "X3")
        m.text = "Umm [S.1] only"
        m.curRefsInUse should be(Map("S.1" -> "X1"))
      }

      it("should clear the label<->id map when no matches") {
        val m = new MutableTextWithStepRefs(null, () => StepState1)
        m.curRefsInUse = Map("S.1" -> "X1", "S.3" -> "X3")
        m.text = "nothing"
        m.curRefsInUse should be('empty)
      }
    }

    describe("transformations") {
      def test(input: String, expectedOutput: String = null) {
        val m = new MutableTextWithStepRefs(null, () => StepState1)
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

  describe("Receiving a StepChangeMsg") {

    def assertMessageDoesNothing(setup: MutableTextWithStepRefs => Unit) {
      val msgCentre = mock[MessageCentre]
      val m = new MutableTextWithStepRefs(msgCentre, () => StepState2)
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
        assertMessageDoesNothing { m =>
          m.curRefsInUse = Map("S.2" -> "X2")
          m.curRefLookup = StepState1
        }
      }
    }

    describe("when the ref lookup table is already up-to-date") {
      it("should do nothing") {
        assertMessageDoesNothing { m =>
          m.curRefsInUse = Map("S.1" -> "X1")
          m.curRefLookup = StepState2
        }
      }
    }

    def newSubject(initialText: String, initialRefsInUse: Map[String, String]) = {
      val comet = mock[CometActor]
      val msgCentre = new MessageCentre(comet)
      val m = new MutableTextWithStepRefs(msgCentre, () => StepState2)
      m._text = initialText
      m.curRefsInUse = initialRefsInUse
      m.curRefLookup = StepState1
      m.sendStepChangeMsg
      m
    }

    def textWasUpdated(subject: => MutableTextWithStepRefs, newText: String, newRefsInUse: Map[String, String]) {
      it("should update the text") {
        subject.text should be(newText)
      }
      it("should update the internal ref->id map") {
        subject.curRefsInUse should be(newRefsInUse)
      }
      it("should record the last used ref lookup table") {
        subject.curRefLookup should be theSameInstanceAs (StepState2)
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

  describe("Step recognition and transformation") {

    def testTransformation(before: String, expectedAfter: String) {
      val refLookupProvider = new RefLookupProvider(StepState1)
      val comet = mock[CometActor]
      val msgCentre = new MessageCentre(comet)
      val m = new MutableTextWithStepRefs(msgCentre, refLookupProvider.value _)
      m.text = before
      m.text.replaceAll("\\s+","") should be(before.replaceAll("\\s+",""))
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
