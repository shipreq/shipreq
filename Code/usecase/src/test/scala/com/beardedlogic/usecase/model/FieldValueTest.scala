package com.beardedlogic.usecase
package model

import test.TestDatabaseSupport
import net.liftweb.util.Helpers._
import org.scalatest.FunSpec
import lib.{UCEditorState, Defaults}
import lib.field._
import lib.StepTree.{Step => Step2, _}

class FieldValueTest extends FunSpec with TestDatabaseSupport {

  def getFieldKey(t: FieldKeyType) = Defaults.FieldList.fieldKeys.find(_.fieldKeyType == t).get

  def textFieldKey = getFieldKey(FieldKeyType.Text)
  def newTextField = textFieldKey.fieldDef.newFieldInstance(new UCEditorState(null), textFieldKey).asInstanceOf[TextField]

  def ncacFieldKey = getFieldKey(FieldKeyType.NormalAndAlternateCourses)
  def newNcAcField = ncacFieldKey.fieldDef.newFieldInstance(new UCEditorState(null), ncacFieldKey).asInstanceOf[NormalAndAlternateCourseFields]

  describe("Text fields") {
    it("should insert when has text") {
      val tf = newTextField
      tf.value.setTextFromUser("Yay!")
      val fv = assertTableDiffs("field_value" -> 1, "value" -> 1, "data" -> 1) {
        db.createFieldValue(tf :: Nil)
      }
      fv.size should be(1)
      fv.head.fieldKey should be(textFieldKey)
      fv.head.fieldData should be(Some("Yay!"))
    }

    it("should NOP when text is blank") {
      val tf = newTextField
      tf.value.setTextFromUser("")
      val fv = assertTableDiffs() { db.createFieldValue(tf :: Nil) }
      fv should be('empty)
    }
  }

  describe("Course fields") {
    it("should insert") {
      val f = newNcAcField
      f.courses =
        StepNode(nextFuncName, 0, f.ncLabelPrefix, 0, Step2("I'm the title"), (
          new StepNode(nextFuncName, 1, 1, Step2("First")) ::
            new StepNode(nextFuncName, 1, 2, NewStep) ::
            new StepNode(nextFuncName, 1, 3, Step2("Finally"), (
              new StepNode(nextFuncName, 2, 1, Step2("Sweet")) :: Nil
              )) :: Nil
          )) :: Nil

      val fv = assertTableDiffs("field_value" -> 1, "step" -> 5, "value" -> 6, "data" -> 6, "relation" -> 5) {
        db.createFieldValue(f :: Nil)
      }
      fv.size should be(1)
      fv.head.fieldKey should be(ncacFieldKey)
      fv.head.fieldData should be(None)
    }
  }
}