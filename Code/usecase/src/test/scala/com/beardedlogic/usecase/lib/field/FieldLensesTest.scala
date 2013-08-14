package com.beardedlogic.usecase.lib.field

import org.scalatest.FunSpec
import com.beardedlogic.usecase.lib.{UseCase, UseCaseHeader}
import com.beardedlogic.usecase.lib.text.FreeText
import com.beardedlogic.usecase.test.NodeUtils._
import com.beardedlogic.usecase.test.{TestData, TestHelpers}

class FieldLensesTest extends FunSpec with TestHelpers with TestData {
  import MockUc1._

  def testFieldValueSet[F <: Field {type Value = V}, V](before: UseCase)(field: F, newValue: V)(after: UseCase) {
    field(after.fieldValues) should be(newValue)
    after.fieldValues should be(before.fieldValues + (field ~> newValue))
  }

  def aLowLevelOperation(set: UseCase => UseCase) {
    it("should not recalculate the steps-and-labels bimap") {
      val uc1 = sampleUC
      val uc2 = set(uc1)
      uc2.stepsAndLabels should be theSameInstanceAs (uc1.stepsAndLabels)
      uc2 should not be theSameInstanceAs(uc1)
    }
  }

  describe("title") {
    it("should get the title") {
      lens.title.get(sampleUC) should be("YES!")
    }
    it("should set the title") {
      val uc = lens.title.set(sampleTextOnlyUC, "No")
      uc.header should be(UseCaseHeader("No", 7))
      uc should be(sampleTextOnlyUC.copy(header = uc.header))
    }
    it should behave like aLowLevelOperation(lens.title =>= {_ + "!"})
  }

  describe("number") {
    it("should get the number") {
      lens.number.get(sampleUC) should be(7)
    }
    it("should set the number") {
      val uc = lens.number.set(sampleTextOnlyUC, 3)
      uc.header should be(UseCaseHeader("YES!", 3))
      assertUseCasesMatchIgnoringStepsAndLabels(uc, sampleTextOnlyUC.copy(header = uc.header))
    }
    it should behave like aLowLevelOperation(lens.number =>= {_ => 123})
  }

  describe("textField") {
    it("should get the text field value") {
      TF1.lens.get(sampleUC) should be(freeText("blah"))
      TF2.lens.get(sampleUC) should be(FreeText.empty)
      TF3.lens.get(sampleUC) should be(freeText("hehe"))
    }
    it("should set the text field value") {
      val newValue = freeText("cool")
      val uc = TF1.lens.set(sampleUC, newValue)
      testFieldValueSet(sampleUC)(TF1, newValue)(uc)
      TF3(uc.fieldValues) should be(freeText("hehe"))
      uc should be(sampleUC.copy(fieldValues = uc.fieldValues))
    }
    it should behave like aLowLevelOperation(TF1.lens.set(_, freeText("woah")))
  }

  describe("stepField") {
    def newValue = parseStepTree("1.0. A\n  1. _\n  2. Good").toStepFieldValue(NCF)
    it("should get the text field value") {
      NCF.lens.get(sampleUC) should be(NcSteps.toStepFieldValue(NCF))
      ECF.lens.get(sampleUC) should be(EcSteps.toStepFieldValue(ECF))
      NCF.lens.get(sampleTextOnlyUC) should be(NCF.empty)
    }
    it("should set the text field value") {
      val uc = NCF.lens.set(sampleUC, newValue)
      testFieldValueSet(sampleUC)(NCF, newValue)(uc)
      assertUseCasesMatchIgnoringStepsAndLabels(uc, sampleUC.copy(fieldValues = uc.fieldValues))
    }
    it should behave like aLowLevelOperation(NCF.lens.set(_, newValue))
  }
}
