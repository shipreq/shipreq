package com.beardedlogic.usecase.lib

import org.scalatest.FunSpec
import Lenses._
import field.Field
import text.FreeText
import com.beardedlogic.usecase.db.UseCaseHeader
import com.beardedlogic.usecase.test.NodeUtils._
import com.beardedlogic.usecase.test.{TestData, TestHelpers}
import Types._

class FieldLensesTest extends FunSpec with TestHelpers with TestData {
  import MockUc1._
  implicit def autotagV[T <: AnyRef](t: T): T @@ Validated = t.tag[Validated]

  def testFieldValueSet[F <: Field {type Value = V}, V](before: UseCase)(field: F, newValue: V)(after: UseCase) {
    field(after.fieldValues) ==== newValue
    after.fieldValues ==== (before.fieldValues + (field ~> newValue))
  }

  def aLowLevelOperation(set: UseCase => UseCase) {
    it("should not recalculate the steps-and-labels bimap") {
      val uc1 = sampleUC
      val uc2 = set(uc1)
      uc2.stepsAndLabels shouldBe theSameInstanceAs (uc1.stepsAndLabels)
      uc2 should not be theSameInstanceAs(uc1)
    }
  }

  describe("title") {
    it("should get the title") {
      ucTitleL.get(sampleUC) ==== "YES!"
    }
    it("should set the title") {
      val uc = ucTitleL.set(sampleTextOnlyUC, "No")
      uc.header ==== UseCaseHeader("No")
      uc ==== sampleTextOnlyUC.copy(header = uc.header)
    }
    it should behave like aLowLevelOperation(ucTitleL =>= {_ + "!"})
  }

  describe("textField") {
    it("should get the text field value") {
      TF1.lens.get(sampleUC) ==== freeText("blah")
      TF2.lens.get(sampleUC) ==== FreeText.empty
      TF3.lens.get(sampleUC) ==== freeText("hehe")
    }
    it("should set the text field value") {
      val newValue = freeText("cool")
      val uc = TF1.lens.set(sampleUC, newValue)
      testFieldValueSet(sampleUC)(TF1, newValue)(uc)
      TF3(uc.fieldValues) ==== freeText("hehe")
      uc ==== sampleUC.copy(fieldValues = uc.fieldValues)
    }
    it should behave like aLowLevelOperation(TF1.lens.set(_, freeText("woah")))
  }

  describe("stepField") {
    def newValue = parseStepTree("1.0. A\n  1. _\n  2. Good").toStepFieldValue(NCF)
    it("should get the text field value") {
      NCF.lens.get(sampleUC) ==== NcSteps.toStepFieldValue(NCF)
      ECF.lens.get(sampleUC) ==== EcSteps.toStepFieldValue(ECF)
      NCF.lens.get(sampleTextOnlyUC) ==== NCF.empty
    }
    it("should set the text field value") {
      val uc = NCF.lens.set(sampleUC, newValue)
      testFieldValueSet(sampleUC)(NCF, newValue)(uc)
      assertUseCasesMatchIgnoringStepsAndLabels(uc, sampleUC.copy(fieldValues = uc.fieldValues))
    }
    it should behave like aLowLevelOperation(NCF.lens.set(_, newValue))
  }
}
