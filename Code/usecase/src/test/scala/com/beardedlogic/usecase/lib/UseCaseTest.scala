package com.beardedlogic.usecase
package lib

import org.scalatest.FunSpec
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation

import change.Changes.StepTextChanged
import change.{NoChange, Change}
import field._
import model.{UseCaseHeader, DataType}
import test.{LoadedTestData, TestDatabaseSupport, TestData, TestHelpers}
import text.{FlowToClause, FlowFromClause, StepText, FreeText}
import Types._
import UseCaseFns._

class UseCaseTest extends FunSpec with TestHelpers with TestData {

  describe("filter()") {
    it("should filter a field list by TextField") {
      val x = filter[TextField](FL)
      x should be(List(TF1, TF2, TF3))
    }
  }

  describe("correctStepsAndLabelsAfterUpdate()") {
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
      assertDoesntRecalc(lens.title.set(_, "asdfklghj"))
    }
    it("should not recalc when only a step text changes") {
      assertDoesntRecalc(uc => lens.stepText.mod(
        _.update("okyjidf")(uc.stepsAndLabels).gimme, (uc, NCF, NcSfv.textmap.keySet.head)
      ))
    }
    it("should not recalc when only a text field changes") {
      assertDoesntRecalc(uc => TF1.lens.mod(_.update("okyjidf")(uc.stepsAndLabels).gimme, uc))
    }

    def assertRecalc(mod: UseCase => UseCase) {
      val uc1 = mod(sampleUC)
      val uc2 = correctStepsAndLabelsAfterUpdate(sampleUC, uc1)
      uc2 should not be theSameInstanceAs(uc1)
      uc2.stepsAndLabels.get should not be (uc1.stepsAndLabels.get)
      uc2.copy(stepsAndLabels = uc1.stepsAndLabels) should be(uc1)
    }
    it("should recalc when the UC number changes") {
      assertRecalc(lens.number.set(_, 654))
    }
  }

  describe("respondToChanges()") {
    case object FakeChange extends Change

    it("should return NoChange if no changes") {
      sampleUC.respondToChanges(FakeChange.asOnlyChange) should be(NoChange)
    }

    it("should return a new UC if a FreeText changes") {
      val original = MockUc2a.UC
      val updated = lens.stepField.set((original, NCF), MockUc2b.NcSfv).regenerateStepsAndLabels
      TF1.lens.get(updated) should not be (MockUc2b.TFV1)
      val done = updated.respondToChanges(MockExistingStepLabelsChanged.asOnlyChange).gimme
      TF1.lens.get(done) should be(MockUc2b.TFV1)
    }
  }

  describe("title change") {
    describe("Responding to changes") {
      describe("A title change") {

        implicit def stepsAndLabels = EmptyStepAndLabelBiMap

        def sfvWithText(f: StepField, stepText: StepText) =
          StepFieldValue(f, StepTree(StepNode(X1, 0, 0, Nil) :: Nil), Map(X1 -> stepText))

        def testChanges(f: StepField, stepTextBefore: StepText) {
          val v1 = sfvWithText(f, stepTextBefore)
          val expected = StepText(X1, freeText("Goat"), stepTextBefore.flowFromClause, stepTextBefore.flowToClause)
          val (uc2, c) = ucWithValues(f -> v1).updateTitle("Goat").openChange
          f(uc2.fieldValues) should be(v1.copy(textmap = Map(X1 -> expected)))
          c.map(_._2) should contain(StepTextChanged(X1))
        }

        def testDoesntChange(f: StepField, stepTextBefore: StepText) {
          val v1 = sfvWithText(f, stepTextBefore)
          val (uc2, c) = ucWithValues(f -> v1).updateTitle("Goat").openChange
          f(uc2.fieldValues) should be(v1)
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
      }
    }
  }

  describe("updateStepFieldText()") {
    it("should update the step text") {
      val uc = NCF.updateText(X2, "great")(MockUc2b.UC).gimme
      NCF.lens.get(uc).textmap should be(MockUc2b.NcStepText + (X2 -> StepText(X2, freeText("great"), None, None)))
    }
    it("should cause other steps to mirror flows") {
      val uc = NCF.updateText(X2, " greater --> [ 7.0] ")(MockUc2b.UC).gimme
      val textmap = NCF.lens.get(uc).textmap
      textmap(X2) should be(StepText(X2, freeText("greater"), None, flowToClause(X1 -> "7.0".asLabel)))
      textmap(X1) should be(StepText(X1, freeText("I'm the root"), flowFromClause(X2 -> "7.0.2".asLabel), None))
    }
  }

  describe("updateTextFieldText()") {
    def test(f: TextField) {
      val uc = f.updateText("The Refusal, Karnivool [7.0]")(MockUc2b.UC).gimme
      f.lens.get(uc) should be (FreeText("The Refusal, Karnivool [7.0]", Map(X1 -> "7.0".asLabel)))
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
      tree.nodes.size should be(expecetedTopLevel)
      val newNode = tree.nodes.last
      newNode.copy(id = null) should be(StepNode(null, 0, expectedLabelIndex, Nil))
      uc2.stepsAndLabels.get.ab(newNode.id) should be(expectedLabel)
      assertStepsAndLabelsRegen(uc2)
    }

    it("should add 7.1 when NC has 7.0") {
      test(MockUc2a.UC, NCF, 2, 1, "7.1")
    }

    it("should add 7.E.1 when EC is empty") {
      test(sampleTextOnlyUC, ECF, 1, 1, "7.E.1")
    }

    it("should add 7.E.3 when EC has 7.E.2") {
      test(sampleUC, ECF, 3, 3, "7.E.3")
    }
  }

  describe("addStep()") {
    lazy val uc = NCF.addStep(X1)(MockUc2b.UC).gimme // insert 7.0.1
    lazy val v = NCF.lens.get(uc)
    it("should add a step") {
      v.tree.sizeRecursive should be(5)
      v.tree.size should be(1)
      v.tree(0).children.size should be(3)
    }
    it("should should have blank text") {
      lens.stepText.get(uc, NCF, v.tree(0)(0).id).isEmpty should be(true)
    }
    it("should update the step label map") {
      assertStepsAndLabelsRegen(uc)
    }
    it("should update refs in text") {
      TF1.lens.get(uc) should be(FreeText("Linking to [7.0.3]", Map(X2 -> "7.0.3".asLabel)))
    }
  }

  describe("removeStep()") {
    lazy val uc = NCF.removeStep(X2)(MockUc2b.UC).gimme // delete 7.0.2
    lazy val v = NCF.lens.get(uc)
    it("should remove the step from the tree") {
      v.tree.sizeRecursive should be (2)
    }
    it("should remove the step text of deleted node and children") {
      v.textmap.keySet should be(Set(X1, X3))
    }
    it("should update the step label map") {
      uc.stepsAndLabels.get.ab should be (Map(X1 -> "7.0".asLabel, X3 -> "7.0.1".asLabel))
    }
    it("should update refs to the step") {
      TF1.lens.get(uc) should be (FreeText("Linking to [DELETED]",Map.empty))
    }
    it("should not allow removal of the root NC node") {
      NCF.removeStep(X1)(MockUc2a.UC) should be (NoChange)
    }
    it("should allow removal of the root NC node") {
      ECF.removeStep(EcSfv.tree(0).id)(sampleUC).toString should startWith("Changed(")
    }
  }
}

// =====================================================================================================================

class UseCaseTest2 extends FunSpec with TestDatabaseSupport with TestHelpers with LoadedTestData {

  def loadByValueId(id: Long): UseCaseSaveCheckpoint = {
    val rec = db.findUseCase(id).get
    Locks.UseCase.withReadLockToken(rec.dataId)(load(rec, db, _))
  }

  def reload(cp: UseCaseSaveCheckpoint) = loadByValueId(cp.rec.valueId)

  describe("Loading") {
    it("should load a simple, manually-saved UC") {
      // Create UC
      val uc = db.createInitialValue(DataType.UseCase)
      val ucValueId = uc.valueId
      sqlu"INSERT INTO usecase VALUES(${ucValueId}, 'ahh', 3, ${FLRec.valueId})".execute

      // Create Text FV
      val txt_fv = db.createInitialValue(DataType.FieldValue)
      db.createFieldValue(txt_fv, TF1, Some("Hehe!"))
      db.relate_usecase_has_fieldValue(uc, txt_fv)

      // Create course FV
      val c_fv = db.createInitialValue(DataType.FieldValue)
      val s1, s2 = db.createInitialValue(DataType.Step)
      db.createFieldValue(c_fv, NCF, None)
      db.createStep(s1, "Root")
      db.createStep(s2, "Child")
      db.relate_usecase_has_fieldValue(uc, c_fv)
      db.relate_stepParent_has_step(c_fv, 0, s1)
      db.relate_stepParent_has_step(s1, 0, s2)

      // Load
      val loaded = loadByValueId(ucValueId).uc

      // Verify
      loaded.header should be(UseCaseHeader("ahh", 3))
      TF1(loaded.fieldValues).text should be("Hehe!")
      assertStepTree(loaded, NCF, "3.0. Root\n  1. Child")
    }

    it("should load a manually-saved UC with refs") {
      // Create UC
      val uc = db.createInitialValue(DataType.UseCase)
      val ucValueId = uc.valueId
      sqlu"INSERT INTO usecase VALUES(${ucValueId}, 'ahh', 3, ${FLRec.valueId})".execute

      // Create course FV
      val c_fv = db.createInitialValue(DataType.FieldValue)
      val s1, s2, s3 = db.createInitialValue(DataType.Step)
      db.createFieldValue(c_fv, NCF, None)
      db.createStep(s1, "Root")
      db.createStep(s2, s"Child [D.${s1.dataId}]")
      db.createStep(s3, s"Other [D.${s2.dataId}]")
      db.relate_usecase_has_fieldValue(uc, c_fv)
      db.relate_stepParent_has_step(c_fv, 0, s1)
      db.relate_stepParent_has_step(c_fv, 1, s3)
      db.relate_stepParent_has_step(s1, 0, s2)

      // Create Text FV
      val txt_fv = db.createInitialValue(DataType.FieldValue)
      db.createFieldValue(txt_fv, TF3, Some(s"look at [D.${s2.dataId}] and [D.${s3.dataId}]!"))
      db.relate_usecase_has_fieldValue(uc, txt_fv)

      // Load
      val loaded = loadByValueId(ucValueId).uc

      // Verify
      loaded.header should be(UseCaseHeader("ahh", 3))
      TF3(loaded.fieldValues).text should be("look at [3.0.1] and [3.1]!")
      assertStepTree(loaded, NCF, "3.0. Root\n  1. Child [3.0]\n3.1. Other [3.0.1]")
    }
  }

  describe("Saving") {
    describe("First-time save") {

      it("should save when empty") {
        assertTableDiffs('usecase -> 1, 'data -> 1, 'value -> 1) {save(EmptyUcWithoutNCF, None, db)}
      }

      it("should return a valid ctx") {
        val cp = save(EmptyUcWithoutNCF, None, db)
        cp should not be ('empty)
      }

      it("should save with 2 text fields") {
        assertTableDiffs('usecase -> 1, 'data -> 3, 'value -> 3, 'field_value -> 2, 'relation -> 2) {
          save(removeNcField(sampleTextOnlyUC), None, db)
        }
      }
    }

    describe("Incremental updates") {
      def testUpdateSucceeds(mutate: UseCase => UseCase, expectedTableDiffs: (Symbol, Int)*) {
        val uc1 = sampleUC
        val cp1 = save(uc1, None, db)
        cp1 should not be (None)
        val uc2 = mutate(uc1)
        val cp2 = assertTableDiffs(expectedTableDiffs: _*) {save(uc2, cp1, db)}
        cp2 should not be (None)
      }

      it("should do nothing when no changes") {
        val uc1 = sampleUC
        val cp1 = save(uc1, None, db)
        cp1 should not be (None)
        assertTableDiffs() {save(uc1, cp1, db)} should be(None)
      }

      it("should save a title change") {
        testUpdateSucceeds(lens.title.set(_, "zzzzzzzzz"), 'usecase -> 1, 'value -> 1, 'relation -> FVs)
      }

      it("should save a UC-number change") {
        testUpdateSucceeds(lens.number.set(_, 666), 'usecase -> 1, 'value -> 1, 'relation -> FVs)
      }

      it("should save a text update") {
        testUpdateSucceeds(uc => TF1.lens.set(uc, freeText("jjj"))
          , 'usecase -> 1, 'field_value -> 1, 'value -> 2, 'relation -> FVs)
      }

      it("should save a text removal") {
        testUpdateSucceeds(uc => TF1.lens.set(uc, FreeText.empty)
          , 'usecase -> 1, 'value -> 1, 'relation -> FVsPlus(-1))
      }

      it("should save a new text") {
        testUpdateSucceeds(uc => TF4.lens.set(uc, freeText("jjj"))
          , 'usecase -> 1, 'field_value -> 1, 'value -> 2, 'data -> 1, 'relation -> FVsPlus(1))
      }
    }
  }

  describe("Saving then Loading") {
    it("should load in full after saving") {
      // Save first
      val valueRows = countRowsIn('value)
      val saved = save(sampleUC, None, db).get
      (countRowsIn('value) - valueRows) should be > 10

      // Then load back (testing manually)
      val loaded = loadByValueId(saved.rec.valueId).uc
      TF1.lens.get(loaded).text should be("blah")
      TF2.lens.get(loaded).text should be("")
      TF3.lens.get(loaded).text should be("hehe")
      NCF.getTextTree(loaded) should matchTree(NcSteps)
      ECF.getTextTree(loaded) should matchTree(EcSteps)
      loaded.header should be (saved.uc.header)
    }

    def reverseTopLevelNodes(f: StepField, uc: UseCase): UseCase = {
      def reverse(v: StepFieldValue) = v.copy(tree = StepTree(fixTopLevelIndices(v.tree.nodes.reverse)))
      NCF.lens.mod(reverse, uc).regenerateStepsAndLabels.afterRespondingToChange(MockExistingStepLabelsChanged)
    }

    it("should load in full after multiple updates") {
      var prevSave: UseCaseSaveCheckpoint = save(sampleUC, None, db).get
      def uc = prevSave.uc
      def ncTreeSize = NCF.lens.get(uc).tree.size

      def testUpdate(mutate: UseCase => UseCase, expectedTableDiffs: (Symbol, Int)*) = {
        val newUc = mutate(prevSave.uc)
        val cpSaveOp = assertTableDiffs(expectedTableDiffs: _*) {save(newUc, Some(prevSave), db)}
        if (expectedTableDiffs.isEmpty) {
          cpSaveOp should be(None)
        } else {
          val cpLoad = reload(cpSaveOp.get)
          assertUseCasesMatch(cpLoad.uc, newUc)
          prevSave = cpSaveOp.get
        }
      }

      // Change title
      testUpdate(lens.title.set(_, "zzzzzzzzz"), 'usecase -> 1, 'value -> 1, 'relation -> FVs)

      // Change text field
      testUpdate(uc => TF1.lens.set(uc, freeText("jjj"))
        ,'usecase -> 1, 'field_value -> 1, 'value -> 2, 'relation -> FVs)

      // Clear text field
      testUpdate(uc => TF1.lens.set(uc, FreeText.empty)
        ,'usecase -> 1, 'value -> 1, 'relation -> FVsPlus(-1))

      // Restore text field
      // TODO A new FV is created when text is deleted and restored. Should it not maintain the FV audit trail?
      testUpdate(uc => TF1.lens.set(uc, freeText("Back!"))
        , 'usecase -> 1, 'field_value -> 1, 'value -> 2, 'data -> 1, 'relation -> FVs)

      // Reorder @ L1
      testUpdate(uc => reverseTopLevelNodes(NCF, uc)
        , 'usecase -> 1, 'field_value -> 1, 'value -> 2, 'relation -> FVsPlus(ncTreeSize))

      // Step text change @ L2
      // 7.0.1: New step + value -- S:1 V:1
      // 7.0: New step + value   -- S:1 V:1
      // 7.0 has 3 children      --         R:3
      // FV has 2 steps          --     V:1 R:2
      // Usecase + value         --     V:1
      testUpdate(uc => {
        val stepId = NCF.lens.get(uc).tree(1)(0).id
        lens.stepText.mod(_.update("Roar.")(uc.stepsAndLabels).gimme, (uc, NCF, stepId))
      }, 'usecase -> 1, 'field_value -> 1, 'step -> 2, 'value -> 4, 'relation -> FVsPlus(5))

      // Ref to new (empty) step
      // 7.2: New step  -- S:1 V:1 D:1
      // Text update    --     V:1     FV:1
      // FV has 3 steps --     V:1     FV:1 R:3
      // UC             --     V:1          R:FVs
      testUpdate(uc => {
        val uc2 = NCF.addTailStep(uc).gimme
        val newText = "New step is [7.2]"
        val uc3 = TF1.lens.mod(t => t.update(newText)(uc2.stepsAndLabels).gimme, uc2)
        TF1.lens.get(uc3).text should be(newText)
        uc3
      }, 'usecase -> 1, 'field_value -> 2, 'step -> 1, 'value -> 4, 'data -> 1, 'relation -> FVsPlus(3))

      // Reorder to step referred to by others
      testUpdate(uc => {
        val uc2 = reverseTopLevelNodes(NCF, uc)
        TF1.lens.get(uc2).text should be("New step is [7.0]")
        uc2
      }, 'usecase -> 1, 'field_value -> 1, 'value -> 2, 'relation -> FVsPlus(ncTreeSize))

      // NOP update to text with ref
      testUpdate(uc => TF1.updateText("New step is [7.0]")(uc).getOrElse(uc))
    }

    it("should normalise and de-normalise refs in text") {
      // Save first
      val a = TF1.updateText("Text like [7.0]")(sampleUC).gimme
      val saved = NCF.updateText(NcSfv.tree(0).id, "Step like [7.0.1]")(a).gimme
      val cp = save(saved, None, db).get

      // Confirm stored normalised in DB
      sql"select text from field_value where text like ${"Text like%"}".as[String].first should not be("Text like [7.0]")
      sql"select text from step where text like ${"Step like%"}".as[String].first should not be("Step like [7.0.1]")

      // Then load back
      val loaded = reload(cp).uc
      val ltree = NCF.lens.get(loaded).tree
      TF1.lens.get(loaded).text should be("Text like [7.0]")
      lens.stepText.get(loaded, NCF, ltree(0).id).text should be("Step like [7.0.1]")
    }

    it("should normalise and de-normalise refs in flow") {
      // Save first
      val saved = NCF.updateText(NcSfv.tree(1).id, "Flow like --> [7.0.1]")(sampleUC).gimme
      val cp = save(saved, None, db).get

      // Confirm stored normalised in DB
      sql"select text from step where text like ${"%⬅%"}".as[String].first should not include("[7.")
      sql"select text from step where text like ${"%➡%"}".as[String].first should not include("[7.")

      // Then load back
      val loaded = reload(cp).uc
      val ltree = NCF.lens.get(loaded).tree
      lens.stepText.get(loaded, NCF, ltree(1).id).text should be("Flow like ➡ [7.0.1]")
      lens.stepText.get(loaded, NCF, ltree(0)(0).id).text should be("First ⬅ [7.1]")
    }
  }

  describe("Real-life failures") {
    it("Save wiping all unchanged fields") {
      truncateAll
      runSqlScript("testdata-01.sql")

      // Load UC, should have 15 steps in total
      val origValueId = 1043
      val cp1 = loadByValueId(origValueId)
      cp1.uc.stepsAndLabels.get.ab.size should be(15)

      // Change 'Frequency of Use' field & save
      val newFreqValue = "P(50%): Never (provided a default project is created on signup, else once).\nP(90%): 0 to 10 times."
      val freqField = cp1.uc.fieldValues.find{
        case (_,t: FreeText) => t.text.startsWith("P(50%): 0 times")
        case _ => false
      }.get._1.asInstanceOf[TextField]
      val uc2 = freqField.lens.set(cp1.uc, freeText(newFreqValue))
      val cp2 = save(uc2, Some(cp1), db)

      // Reload
      val newValueId  = db.findLatestUseCaseByValueId(origValueId).get.valueId
      newValueId should not be (origValueId)
      val reloaded = loadByValueId(newValueId).uc

      // Confirm data
      reloaded.stepsAndLabels.get.ab.size should be(15)
      freqField.lens.get(reloaded).text should be(newFreqValue)
    }
  }
}