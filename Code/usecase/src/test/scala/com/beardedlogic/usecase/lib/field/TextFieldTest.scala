package com.beardedlogic.usecase
package lib
package field

import org.scalatest.FunSpec
import org.mockito.Mockito._
import model._
import msg.MessageCentre
import TypeTags._
import test.TestHelpers

class TextFieldTest extends FunSpec with TestHelpers {

  def sampleTextField = {
    val tf = new TextField(mock[TextFieldDef], mockUseCaseCtx, mock[FieldKey])
    when(tf.ucCtx.savedSteps).thenReturn(BiMap(111.tag[StepDataId] -> "X1".asLocalStepId, 222.tag[StepDataId] -> "X2".asLocalStepId))
    when(tf.ucCtx.stepLabelMap).thenReturn(BiMap.flatten("X1" -> "4.1", "X2" -> "4.2"))
    tf.init
    tf
  }

  describe("Setting state") {
    it("should accept simple text") {
      val tf = sampleTextField
      tf.value.refsInText += ("A" -> "B".asLocalStepId)

      tf.setState("Hehe!".hasNormalisedRefs)()

      tf.value.text should be("Hehe!")
      tf.value.refsInText should be('empty)
    }

    it("should accept text with normalised refs") {
      val tf = sampleTextField
      val fn = tf.setState("Hehe! [D.100]".hasNormalisedRefs)
      when(tf.ucCtx.savedSteps).thenReturn(BiMap(100.tag[StepDataId] -> "X1".asLocalStepId))
      when(tf.ucCtx.stepLabelMap).thenReturn(BiMap.flatten("X1" -> "5.4"))
      fn()

      tf.value.text should be("Hehe! [5.4]")
      tf.value.refsInText should be(Map("5.4" -> "X1"))
    }
  }

  describe("Saving") {
    describe("save_?()") {
      it("should not save when no text") {
        val tf = sampleTextField
        tf.save_? should be(false)
      }
      it("should save when has text") {
        val tf = sampleTextField
        tf.value.setTextFromUser("Hello!")
        tf.save_? should be(true)
      }
    }

    def testPresave(tf: TextField, lastSave: Option[(FieldSaveCtx, String @@ NormalisedRefs)], expectChange: Boolean) {
      val saveCtx = mock[MutableFieldSaveCtx]
      val dao = mock[DAO]
      tf.save_? should be(true)
      tf.presave(lastSave, saveCtx, dao) should be(expectChange)
      verifyZeroInteractions(saveCtx)
      verifyZeroInteractions(dao)
    }

    describe("presave() with no old state") {
      it("should save simple text") {
        val tf = sampleTextField
        tf.value.setTextFromUser("Hello!")
        testPresave(tf, None, true)
      }
    }

    describe("presave() with old state") {
      it("should save simple text when it differs") {
        val tf = sampleTextField
        tf.value.setTextFromUser("Hello!")
        testPresave(tf, Some(mock[FieldSaveCtx], "ah".hasNormalisedRefs), true)
      }
      it("should not save simple text when unchanged") {
        val tf = sampleTextField
        tf.value.setTextFromUser("Hello!")
        testPresave(tf, Some(mock[FieldSaveCtx], "Hello!".hasNormalisedRefs), false)
      }
      it("should not save text with refs matches unchanged, normalised text") {
        val tf = sampleTextField
        tf.value.setTextFromUser("Hello! [4.1]")
        tf.value.text should be("Hello! [4.1]")
        testPresave(tf, Some(mock[FieldSaveCtx], "Hello! [D.111]".hasNormalisedRefs), false)
      }
      it("should save text with refs matches differs") {
        val tf = sampleTextField
        tf.value.setTextFromUser("Hello! [4.1]")
        tf.value.text should be("Hello! [4.1]")
        testPresave(tf, Some(mock[FieldSaveCtx], "Hello! [D.222]".hasNormalisedRefs), true)
      }
    }

    describe("save()") {
      def testSave(text: String, expectedSaveText: String) {
        val saveCtx = mock[MutableFieldSaveCtx]
        val dao = mock[DAO]
        val tf = sampleTextField
        tf.value.setTextFromUser(text)
        tf.value.text should be(text)
        tf.save_? should be(true)
        tf.presave(None, saveCtx, dao) should be(true)
        val r = tf.save(null, null, dao)
        r._1 should be(Some(expectedSaveText))
        r._2 should be(expectedSaveText)
        verifyZeroInteractions(saveCtx)
        verifyZeroInteractions(dao)
      }

      it("should save simple text") {
        testSave("Hello", "Hello")
      }
      it("should text with normalised refs") {
        testSave("Hello [4.1]", "Hello [D.111]")
      }
    }
  }

  /*
  describe("Loading") {
    val ATextFieldDef = new TextFieldDef("ah")
    val Key_1 = new FieldKey(1, FieldKeyType.Text, Some("AH"))
    val Key_2 = new FieldKey(2, FieldKeyType.Text, Some("AH2"))
    val Value_1 = new FieldValue(10, 1, Some("Jord"))
    val Value_2 = new FieldValue(20, 2, Some("puls"))

    it("should clear value when no field value exists") {
      val tr = new TextField(ATextFieldDef, mock[UseCaseCtx], Key_1)
      tr.value.setTextFromUser("ahness")
      val ctx = new FieldLoadCtx(Map(2L -> Value_2), null, null)
      tr.value.text should not be ('empty)
      tr.load(ctx)
      tr.value.text should be('empty)
    }

    it("should change its value to the loaded field value") {
      val tr1 = new TextField(ATextFieldDef, mock[UseCaseCtx], Key_1)
      val tr2 = new TextField(ATextFieldDef, mock[UseCaseCtx], Key_2)
      val ctx = new FieldLoadCtx(Map(1L -> Value_1, 2L -> Value_2), null, null)
      tr1.load(ctx)
      tr1.value.text should be("Jord")
      tr2.load(ctx)
      tr2.value.text should be("puls")
    }
  }
  */
}