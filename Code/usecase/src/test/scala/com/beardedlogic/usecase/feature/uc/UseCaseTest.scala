package com.beardedlogic.usecase
package feature.uc

import org.scalatest.FunSpec
import scalaz.Name
import lib.Types._
import change.Changes.{TextChanged, StepTextChanged}
import change.{UseCaseUpdater, NoChange}
import db.{FieldKeyRec, FieldKeyType, UseCaseHeader}
import field._
import step.{StepTree, StepNode}
import test.{TestData, TestHelpers}
import text.{FlowToClause, FlowFromClause, StepText, FreeText}
import text.FreeTextTerms._
import util.BiMap
import Lenses._
import UseCaseFns._

class UseCaseTest extends FunSpec with TestHelpers with TestData {

  implicit def autoCtx(sl: StepAndLabelBiMap) = UcParsingCtx.Empty.copy(stepsAndLabels = sl)
  implicit def ucTu(uc: UseCase) = UseCaseUpdater(uc, UseCaseRelations.Empty)

  describe("filter()") {
    it("should filter a field list by TextField") {
      val x = filter[TextField](FL)
      x ==== List(TF1, TF2, TF3)
    }
  }

  describe("correctStepsAndLabelsAfterUpdate()") {
    import MockUc1._

    def assertDoesntRecalc(mod: UseCase => UseCase) {
      val uc1 = mod(sampleUC)
      val uc2 = correctStepsAndLabelsAfterUpdate(sampleUC, uc1)
      uc2 should be theSameInstanceAs (uc1)
    }

    it("should not recalc when both UCs are the same") {
      correctStepsAndLabelsAfterUpdate(sampleUC, sampleUC) should be theSameInstanceAs (sampleUC)
      correctStepsAndLabelsAfterUpdate(sampleUC.copy(), sampleUC) should be theSameInstanceAs (sampleUC)
    }

    it("should not recalc when only the title has changed") {
      assertDoesntRecalc(ucTitleL.set(_, "asdfklghj".validated))
    }

    it("should not recalc when only a step text changes") {
      assertDoesntRecalc(uc => ucStepTextInstL.mod(
        _.update("okyjidf")(uc.stepsAndLabels).gimme, (uc, (NCF, NcSfv.textmap.keySet.head))
      ))
    }

    it("should not recalc when only a text field changes") {
      assertDoesntRecalc(uc => TF1.lens.mod(_.update("okyjidf")(uc.stepsAndLabels).gimme, uc))
    }

    def assertRecalc(mod: UseCase => UseCase) {
      val uc1 = mod(sampleUC)
      val uc2 = correctStepsAndLabelsAfterUpdate(sampleUC, uc1)
      uc2 should not be theSameInstanceAs(uc1)
      uc2.stepsAndLabels.value should not be (uc1.stepsAndLabels.value)
      uc2.copy(stepsAndLabels = uc1.stepsAndLabels) ==== uc1
    }
  }

  describe("respondToChanges()") {

    it("should return NoChange if no changes") {
      MockUc1.sampleUC.respondToChanges(TextChanged.asOnlyChange) ==== NoChange
    }

    it("should return a new UC if a FreeText changes") {
      val original = MockUc2a.UC
      val updated = ucStepFieldL.set((original, NCF), MockUc2b.NcSfv).regenerateStepsAndLabels
      TF1.lens.get(updated) should not be (MockUc2b.TFV1)
      val done = updated.respondToChanges(MockExistingStepLabelsChanged.asOnlyChange).gimme
      TF1.lens.get(done) ==== MockUc2b.TFV1
    }
  }

  describe("title change") {
    describe("Responding to changes") {
      describe("A title change") {

        implicit def ctx = UcParsingCtx.Empty

        def sfvWithText(f: StepField, stepText: StepText) =
          StepFieldValue(f, StepTree(StepNode(X1, 0, 0, Nil) :: Nil), Map(X1 -> stepText))

        def testChanges(f: StepField, stepTextBefore: StepText) {
          val v1 = sfvWithText(f, stepTextBefore)
          val expected = StepText(X1, freeText("Goat"), stepTextBefore.flowFromClause, stepTextBefore.flowToClause)
          val (uc2, c) = ucWithValues(f -> v1).updateTitle("Goat").openChange
          f(uc2.fieldValues) ==== v1.copy(textmap = Map(X1 -> expected))
          c.map(_._2) should contain(StepTextChanged(X1))
        }

        def testDoesntChange(f: StepField, stepTextBefore: StepText) {
          val v1 = sfvWithText(f, stepTextBefore)
          val (uc2, c) = ucWithValues(f -> v1).updateTitle("Goat").openChange
          f(uc2.fieldValues) ==== v1
          c.map(_._2) should not contain(StepTextChanged(X1))
        }

        it("should change the NC root text if it matches the old title") {
          testChanges(NCF, StepText.parse(X1, UCH.title))
        }

        it("should change the NC root text if empty") {
          testChanges(NCF, StepText.empty(X1))
        }

        it("should preserve the NC root text flow when changing") {
          val f = Some(FlowFromClause(Map(X2 -> "X2".asLabel)))
          val t = Some(FlowToClause(Map(X3 -> "X333".asLabel)))
          testChanges(NCF, StepText(X1, freeText(UCH.title), f, t))
        }

        it("should not change the NC root text if it has some other value") {
          testDoesntChange(NCF, StepText.parse(X1, "some other value"))
        }

        it("should not change the EC root text") {
          testDoesntChange(ECF, StepText.empty(X1))
        }

        it("should update self-refs in NC steps") {
          val NCF = NormalCourseField(FieldKeyRec(14.tag[IsFieldKeyId],FieldKeyType.NormalAndAlternateCourses,None))
          val uc1 = UseCase.as((1:Short).tag[IsUseCaseNumber],UseCaseHeader("Hehe".tag[Validated])
            ,List(TextField(TextFieldDefinition("Description"),FieldKeyRec(10.tag[IsFieldKeyId],FieldKeyType.Text,Some("Description")))~>FreeText.empty
            ,TextField(TextFieldDefinition("Actors"),FieldKeyRec(11.tag[IsFieldKeyId],FieldKeyType.Text,Some("Actors")))~>FreeText.empty
            ,TextField(TextFieldDefinition("Pre-Conditions"),FieldKeyRec(12.tag[IsFieldKeyId],FieldKeyType.Text,Some("Pre-Conditions")))~>FreeText.empty
            ,TextField(TextFieldDefinition("Post-Conditions"),FieldKeyRec(13.tag[IsFieldKeyId],FieldKeyType.Text,Some("Post-Conditions")))~>FreeText.empty
            ,NCF~>StepFieldValue(NormalCourseField(FieldKeyRec(14.tag[IsFieldKeyId],FieldKeyType.NormalAndAlternateCourses,None)),StepTree(List(StepNode("wVaEE".tag[IsLocalStepId],0,0,List(StepNode("wEGJZ".tag[IsLocalStepId],1,1,Nil))))),Map(
                "wEGJZ".tag[IsLocalStepId]->StepText("wEGJZ".tag[IsLocalStepId],FreeText(List(PlainText("Link to "),UseCaseSelfRef((1:Short).tag[IsUseCaseNumber],"Hehe"))),None,None),
                "wVaEE".tag[IsLocalStepId]->StepText("wVaEE".tag[IsLocalStepId],FreeText(List(PlainText("Hehe"))),None,None)
              ))
            ,ExceptionCourseField(FieldKeyRec(15.tag[IsFieldKeyId],FieldKeyType.ExceptionCourses,None))~>StepFieldValue(ExceptionCourseField(FieldKeyRec(15.tag[IsFieldKeyId],FieldKeyType.ExceptionCourses,None)),StepTree(Nil),Map())
            ,TextField(TextFieldDefinition("Use Case Relationships"),FieldKeyRec(16.tag[IsFieldKeyId],FieldKeyType.Text,Some("Use Case Relationships")))~>FreeText.empty
            ,TextField(TextFieldDefinition("Constraints and Business Rules"),FieldKeyRec(17.tag[IsFieldKeyId],FieldKeyType.Text,Some("Constraints and Business Rules")))~>FreeText.empty
            ,TextField(TextFieldDefinition("Frequency of Use"),FieldKeyRec(18.tag[IsFieldKeyId],FieldKeyType.Text,Some("Frequency of Use")))~>FreeText.empty
            ,TextField(TextFieldDefinition("Special Requirements"),FieldKeyRec(19.tag[IsFieldKeyId],FieldKeyType.Text,Some("Special Requirements")))~>FreeText.empty
            ,TextField(TextFieldDefinition("Assumptions"),FieldKeyRec(20.tag[IsFieldKeyId],FieldKeyType.Text,Some("Assumptions")))~>FreeText.empty
            ,TextField(TextFieldDefinition("Notes and Issues"),FieldKeyRec(21.tag[IsFieldKeyId],FieldKeyType.Text,Some("Notes and Issues")))~>FreeText.empty
            ),Name(BiMap("wEGJZ".tag[IsLocalStepId]->"1.0.1".tag[IsStepLabel],"wVaEE".tag[IsLocalStepId]->"1.0".tag[IsStepLabel])))

          val stepId = "wEGJZ".tag[IsLocalStepId]
          val cr = UseCaseUpdater(uc1, ctx.rels).updateTitle("GREAT")
          val uc2 = cr.gimme
          NCF(uc2.fieldValues).textmap(stepId).text ==== "Link to [UC-1: GREAT]"
          cr.getChanges.map(_._2) should contain(StepTextChanged(stepId))
        }

      }
    }
  }

  describe("updateStepFieldText()") {
    it("should update the step text") {
      val uc = NCF.updateText(X2, "great")(MockUc2b.UC).gimme
      NCF.lens.get(uc).textmap ==== MockUc2b.NcStepText + (X2 -> StepText(X2, freeText("great"), None, None))
    }
    it("should cause other steps to mirror flows") {
      val uc = NCF.updateText(X2, " greater --> [ 7.0] ")(MockUc2b.UC).gimme
      val textmap = NCF.lens.get(uc).textmap
      textmap(X2) ==== StepText(X2, freeText("greater"), None, flowToClause(X1 -> "7.0".asLabel))
      textmap(X1) ==== StepText(X1, freeText("I'm the root"), flowFromClause(X2 -> "7.0.2".asLabel), None)
    }
  }

  describe("updateTextFieldText()") {
    def test(f: TextField) {
      val uc = f.updateText("The Refusal, Karnivool [7.0]")(MockUc2b.UC).gimme
      f.lens.get(uc) ==== FreeText(PlainText("The Refusal, Karnivool ") :: StepRef(X1, "7.0".asLabel) :: Nil)
    }
    it("should update existing text") {
      test(TF1)
    }
    it("should update empty text") {
      test(TF2)
    }
  }

  describe("addTailStep()") {
    def test(uc1: UseCase, f: StepField, expecetedTopLevel: Int, expectedLabelIndex: Int, expectedLabel: String) {
      val uc2 = f.addTailStep(uc1).gimme
      val tree = f.lens.get(uc2).tree
      tree.nodes.size ==== expecetedTopLevel
      val newNode = tree.nodes.last
      newNode.copy(id = null) ==== StepNode(null, 0, expectedLabelIndex, Nil)
      uc2.stepsAndLabels.value.ab(newNode.id) ==== expectedLabel.asLabel
      assertStepsAndLabelsRegen(uc2)
    }

    it("should add 7.1 when NC has 7.0") {
      test(MockUc2a.UC, NCF, 2, 1, "7.1")
    }

    it("should add 7.E.1 when EC is empty") {
      test(MockUc1.sampleTextOnlyUC, ECF, 1, 1, "7.E.1")
    }

    it("should add 7.E.3 when EC has 7.E.2") {
      test(MockUc1.sampleUC, ECF, 3, 3, "7.E.3")
    }
  }

  describe("addStep()") {
    lazy val uc = NCF.addStep(X1)(MockUc2b.UC).gimme // insert 7.0.1
    lazy val v = NCF.lens.get(uc)
    it("should add a step") {
      v.tree.sizeRecursive ==== 5
      v.tree.size ==== 1
      v.tree(0).children.size ==== 3
    }
    it("should should have blank text") {
      ucStepTextInstL.get(uc, (NCF, v.tree(0)(0).id)) shouldBe empty
    }
    it("should update the step label map") {
      assertStepsAndLabelsRegen(uc)
    }
    it("should update refs in text") {
      TF1.lens.get(uc) ==== FreeText(PlainText("Linking to ") :: StepRef(X2, "7.0.3".asLabel) :: Nil)
    }
  }

  describe("removeStep()") {
    lazy val uc = NCF.removeStep(X2)(MockUc2b.UC).gimme // delete 7.0.2
    lazy val v = NCF.lens.get(uc)
    it("should remove the step from the tree") {
      v.tree.sizeRecursive ==== 2
    }
    it("should remove the step text of deleted node and children") {
      v.textmap.keySet ==== Set(X1, X3)
    }
    it("should update the step label map") {
      uc.stepsAndLabels.value.ab ==== Map(X1 -> "7.0".asLabel, X3 -> "7.0.1".asLabel)
    }
    it("should update refs to the step") {
      TF1.lens.get(uc) ==== FreeText(PlainText("Linking to ") :: DeletedRef :: Nil)
    }
    it("should not allow removal of the root NC node") {
      NCF.removeStep(X1)(MockUc2a.UC) ==== NoChange
    }
    it("should allow removal of the root NC node") {
      import MockUc1._
      ECF.removeStep(EcSfv.tree(0).id)(sampleUC).toString should startWith("Changed(")
    }
  }
}
