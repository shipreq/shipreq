package com.beardedlogic.usecase
package lib
package field

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import model.{FieldValue, FieldLoadCtx, FieldKeyType, FieldKey}
import msg.MessageCentre
import TypeTags._

class TextFieldTest extends FunSpec with ShouldMatchers with MockitoSugar {

  describe("Setting state") {
    it("should accept simple text") {
      val ucCtx = mock[UseCaseCtx]
      when(ucCtx.msgCentre).thenReturn(mock[MessageCentre])
      when(ucCtx.stepLabelMapProvider).thenReturn(() => Map.empty[String, String])
      val tf = new TextField(mock[TextFieldDef], ucCtx, mock[FieldKey])
      tf.value.refsInText += ("A" -> "B".asLocalStepId)

      tf.setState("Hehe!".hasNormalisedRefs)()

      tf.value.text should be("Hehe!")
      tf.value.refsInText should be('empty)
    }

    it("should accept text with normalised refs") {
      val ucCtx = mock[UseCaseCtx]
      when(ucCtx.msgCentre).thenReturn(mock[MessageCentre])
      when(ucCtx.stepLabelMapProvider).thenReturn(() => ucCtx.stepLabelMap)
      val tf = new TextField(mock[TextFieldDef], ucCtx, mock[FieldKey])

      val fn = tf.setState("Hehe! [D.100]".hasNormalisedRefs)
      when(ucCtx.savedSteps).thenReturn(Map(100.tag[StepDataId] -> "X1".asLocalStepId))
      when(ucCtx.stepLabelMap).thenReturn(Map("X1" -> "5.4", "5.4" -> "X1"))
      fn()

      tf.value.text should be("Hehe! [5.4]")
      tf.value.refsInText should be(Map("5.4" -> "X1"))
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