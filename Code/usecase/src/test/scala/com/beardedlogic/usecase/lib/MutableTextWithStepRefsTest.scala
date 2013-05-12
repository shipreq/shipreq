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

  val StepState1 = Map("S.1" -> "X1", "S.2" -> "X2", "S.3" -> "X3",
                        "X1" -> "S.1", "X2" -> "S.2", "X3" -> "S.3")

  val StepState2 = Map("S.A" -> "X1", "S.2" -> "X2", "S.4" -> "X4",
                        "X1" -> "S.A", "X2" -> "S.2", "X4" -> "S.4")

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

  describe("Setting a new text value") {
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

    // TODO Should transform invalid step refs
  }

  describe("Upon receiving a StepChangeMsg") {

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

    describe("when there are steps referenced that are affected") {
      lazy val comet = mock[CometActor]
      lazy val subject = {
        val msgCentre = new MessageCentre(comet)
        val m = new MutableTextWithStepRefs(msgCentre, () => StepState2)
        m._text = "Umm [S.1] & [S.2] ah and [S.1]!"
        m.curRefsInUse = Map("S.1" -> "X1", "S.2" -> "X2")
        m.curRefLookup = StepState1
        m.sendStepChangeMsg
        m
      }
      it("should update the text") { subject.text should be("Umm [S.A] & [S.2] ah and [S.A]!") }
      it("should update the internal ref->id map") { subject.curRefsInUse should be(Map("S.A" -> "X1", "S.2" -> "X2")) }
      it("should record the last used ref lookup table") { subject.curRefLookup should be theSameInstanceAs (StepState2) }
      it("should push an update") { verify(comet).!(any[PushToClient]) }
    }
  }

  describe("Step recognition and transformation") {

    def testTransformation(before: String, expectedAfter: String) {
      val refLookupProvider = new RefLookupProvider(StepState1)
      val comet = mock[CometActor]
      val msgCentre = new MessageCentre(comet)
      val m = new MutableTextWithStepRefs(msgCentre, refLookupProvider.value _)
      m.text = before
      m.text should be(before)
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
                            , ("[S.1] hehe [S.1]", "[S.A] hehe [S.A]")
                            , ("[S.2]", "[S.2]")
                            , ("Whatever", "Whatever")
                            , ("And S.1 is blah", "And S.1 is blah")
                          )
      forAll(examples)(testTransformation _)
    }
  }
}
