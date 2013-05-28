package com.beardedlogic.usecase
package lib
package field

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar
import model.{FieldValue, FieldLoadCtx, FieldKeyType, FieldKey}

class TextFieldText extends FunSpec with ShouldMatchers with MockitoSugar {

  describe("load()") {
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
}