package com.beardedlogic.usecase
package model

import test.TestHelpers
import net.liftweb.util.Helpers._
import org.scalatest.FunSpec
import lib.TypeTags._

class FieldValueTest extends FunSpec with TestHelpers {

  implicit def autoTypeStepValues(m: Map[String, PlainValue[DataType.Step]]) = m.asInstanceOf[Map[String @@ LocalStepId, PlainValue[DataType.Step]]]
  def SVMap(pairs: (String,PlainValue[DataType.Step])*) = autoTypeStepValues(Map(pairs:_*))

  val FK1 = new FieldKey(11, FieldKeyType.Text, Some("Ah1"))
  val FK2 = new FieldKey(12, FieldKeyType.Text, Some("Ah2"))
  val FK3 = new FieldKey(13, FieldKeyType.Text, Some("Ah3"))

  val FV1 = new PlainValue[DataType.FieldValue](21,31,1)
  val FV2 = new PlainValue[DataType.FieldValue](22,32,1)
  val FV3 = new PlainValue[DataType.FieldValue](23,33,1)

  val S1 = new PlainValue[DataType.Step](41,51,1)
  val S2 = new PlainValue[DataType.Step](42,52,1)
  val S3 = new PlainValue[DataType.Step](43,53,1)

  describe("Combining FieldSaveCtx") {
    it("should combine all maps") {
      val a = FieldSaveCtx(Map(FK1 -> FV1), SVMap("s1" -> S1))
      val b = FieldSaveCtx(Map(FK2 -> FV2), SVMap("s2" -> S2))
      val c = a.combineWith(b)
      c.fieldValues should be(Map(FK1 -> FV1, FK2 -> FV2))
      c.stepValues should be(SVMap("s1" -> S1, "s2" -> S2))
    }

    it("should not override conflicts") {
      val a = FieldSaveCtx(Map(FK1 -> FV1), SVMap("s1" -> S1))
      val b = FieldSaveCtx(Map(FK1 -> FV2), SVMap("s1" -> S2))
      val c = a.combineWith(b)
      c.fieldValues should be(Map(FK1 -> FV1))
      c.stepValues should be(SVMap("s1" -> S1))
    }
  }

  /*
  def getFieldKey(t: FieldKeyType) = Defaults.FieldList.fieldKeys.find(_.fieldKeyType == t).get

  def textFieldKey = getFieldKey(FieldKeyType.Text)
  def newTextField = textFieldKey.fieldDef.newFieldInstance(new UseCaseCtx(null), textFieldKey).asInstanceOf[TextField]

  def ncacFieldKey = getFieldKey(FieldKeyType.NormalAndAlternateCourses)
  def newNcAcField = ncacFieldKey.fieldDef.newFieldInstance(new UseCaseCtx(null), ncacFieldKey).asInstanceOf[NormalAndAlternateCourseFields]

  def ecFieldKey = getFieldKey(FieldKeyType.ExceptionCourses)
  def newEcField = ecFieldKey.fieldDef.newFieldInstance(new UseCaseCtx(null), ecFieldKey).asInstanceOf[ExceptionCourseFields]

  describe("Text fields") {
    it("should insert when has text") {
      val tf = newTextField
      tf.value.setTextFromUser("Yay!")
      val fv = assertTableDiffs("field_value" -> 1, "value" -> 1, "data" -> 1) {
        db.createInitialFieldValues(tf :: Nil)
      }
      fv.size should be(1)
      fv.head.fieldKeyId should be(textFieldKey.valueId)
      fv.head.fieldData should be(Some("Yay!"))
    }

    it("should NOP when text is blank") {
      val tf = newTextField
      tf.value.setTextFromUser("")
      val fv = assertTableDiffs() { db.createInitialFieldValues(tf :: Nil) }
      fv should be('empty)
    }
  }

  describe("Course fields") {
    def testSave(f: CourseFields) = {
      f.courses =
        StepNode(nextFuncName, 0, 0, Step2("I'm the title"), (
          new StepNode(nextFuncName, 1, 1, Step2("First")) ::
            new StepNode(nextFuncName, 1, 2, NewStep) ::
            new StepNode(nextFuncName, 1, 3, Step2("Finally"), (
              new StepNode(nextFuncName, 2, 1, Step2("Sweet")) :: Nil
              )) :: Nil
          )) :: Nil

      val fv = assertTableDiffs("field_value" -> 1, "step" -> 5, "value" -> 6, "data" -> 6, "relation" -> 5) {
        db.createInitialFieldValues(f :: Nil)
      }
      fv.size should be(1)
      fv.head.fieldKeyId should be(f.fieldKey.valueId)
      fv.head.fieldData should be(None)
    }

    it("should save NC/AC steps") { testSave(newNcAcField) }
    it("should save EC steps") { testSave(newEcField) }

    it("should NOP when no steps") {
      val f = newEcField
      f.courses = Nil

      val fv = assertTableDiffs() { db.createInitialFieldValues(f :: Nil) }
      fv should be('empty)
    }
  }
  */
}
