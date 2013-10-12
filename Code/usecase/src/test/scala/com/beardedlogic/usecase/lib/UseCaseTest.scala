package com.beardedlogic.usecase
package lib

import org.scalatest.FunSpec
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation

import Types._
import change.Changes.{TextChanged, StepTextChanged}
import change.NoChange
import db.UseCaseHeader
import field._
import test.{LoadedTestData, TestDatabaseSupport, TestData, TestHelpers}
import text.{FlowToClause, FlowFromClause, StepText, FreeText}
import Lenses._
import UseCaseFns._
import UseCasePersistence._

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
      f.lens.get(uc) ==== FreeText("The Refusal, Karnivool [7.0]", Map(X1 -> "7.0".asLabel), false)
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
      TF1.lens.get(uc) ==== FreeText("Linking to [7.0.3]", Map(X2 -> "7.0.3".asLabel), false)
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
      TF1.lens.get(uc) ==== freeText("Linking to [DELETED]")
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

// =====================================================================================================================

class UseCaseTest2 extends FunSpec with TestDatabaseSupport with TestHelpers with LoadedTestData {
  import Tables._
  import MockUc1._
  val rels = 2 + 5 + 3 // 2 text fields + 5 NC steps + 3 EC steps

  implicit def autoCtx(sl: StepAndLabelBiMap) = UcParsingCtx.Empty.copy(stepsAndLabels = sl)
  implicit def ucTu(uc: UseCase) = UseCaseUpdater(uc, UseCaseRelations.Empty)

  def loadRev(revId: UseCaseRevId, projectId: ProjectId): UseCaseSaveCheckpoint = {
    val rec = dao.findUseCaseRev(revId).get
    Locks.SingleUseCase.read(rec.identId, projectId)(load(rec, dao, _))
  }

  def reload(cp: UseCaseSaveCheckpoint, projectId: ProjectId) = loadRev(cp.rec, projectId)

  def createInitialTextRev(ucIdentId: UseCaseIdentId, fkId: FieldKeyId, text: String) =
    dao.createTextRev(dao.createTextIdent(ucIdentId, fkId), 1, text.hasNormalisedRefs)

  describe("Loading") {
    it("should set NC.0 to the title for new UCs") {
      val pid = newProjectId()
      val x = createUseCaseIdentAndRev1(pid, UseCaseHeader("Hello".validated))
      val y = loadRev(x, pid)
      val sfv = NCF.lens.get(y.uc)
      sfv.textmap(sfv.tree.head.id).text ==== x.header.title
    }

    it("should load a simple, manually-saved UC") {
      // Create UC
      val pid = newProjectId()
      val ucIdent = dao.createUseCaseIdentWithForcedNumber(pid, (3:Short).tag[IsUseCaseNumber])
      val ucRev = dao.createUseCaseRev(ucIdent, 1, UseCaseHeader("ahh".validated))

      // Create Text FV
      val txtRev = createInitialTextRev(ucIdent, TF1, "Hehe")
      dao.linkUcToText(ucRev, txtRev)

      // Create course FV
      val s1 = createInitialTextRev(ucIdent, NCF, "Root")
      val s2 = createInitialTextRev(ucIdent, NCF, "Child")
      dao.linkUcToStep(ucRev, "3.0".asLabel, 0, None, s1)
      dao.linkUcToStep(ucRev, "3.0.1".asLabel, 0, Some(s1.id), s2)

      // Load
      val loaded = loadRev(ucRev, pid).uc

      // Verify
      loaded.header ==== UseCaseHeader("ahh".validated)
      TF1(loaded.fieldValues).text ==== "Hehe"
      assertStepTree(loaded, NCF, "3.0. Root\n  1. Child")
    }

    it("should load a manually-saved UC with refs") {
      // Create UC
      val pid = newProjectId()
      val ucIdent = dao.createUseCaseIdentWithForcedNumber(pid, (3:Short).tag[IsUseCaseNumber])
      val ucRev = dao.createUseCaseRev(ucIdent, 1, UseCaseHeader("ahh".validated))

      // Create course FV
      val s1 = createInitialTextRev(ucIdent, NCF, "Root")
      val s2 = createInitialTextRev(ucIdent, NCF, s"Child [D.${s1.identId}]")
      val s3 = createInitialTextRev(ucIdent, NCF, s"Other [D.${s2.identId}]")
      dao.linkUcToStep(ucRev, "3.0".asLabel, 0, None, s1)
      dao.linkUcToStep(ucRev, "3.0.1".asLabel, 0, Some(s1.id), s2)
      dao.linkUcToStep(ucRev, "3.1".asLabel, 1, None, s3)

      // Create Text FV
      val txtRev = createInitialTextRev(ucIdent, TF3, s"look at [D.${s2.identId}] and [D.${s3.identId}]!")
      dao.linkUcToText(ucRev, txtRev)

      // Load
      val loaded = loadRev(ucRev, pid).uc

      // Verify
      loaded.header ==== UseCaseHeader("ahh".validated)
      TF3(loaded.fieldValues).text ==== "look at [3.0.1] and [3.1]!"
      assertStepTree(loaded, NCF, "3.0. Root\n  1. Child [3.0]\n3.1. Other [3.0.1]")
    }
  }

  describe("Saving") {
    describe("First-time save") {

      it("should save when empty") {
        val projectId = newProjectId()
        assertTableDiffs(Tables.Usecase -> 1, Tables.UsecaseRev -> 1) {
          saveUseCase(EmptyUcWithoutNCF, None, projectId)
        }
      }

      it("should return a valid ctx") {
        val cp = saveUseCase(EmptyUcWithoutNCF, None, newProjectId())
        cp should not be ('empty)
      }

      it("should save with 2 text fields") {
        // Two revs here:
        // 1) initial save via UseCaseL screen (header only)
        // 2) proper save from UCE (saves fields)
        val projectId = newProjectId()
        assertTableDiffs(Usecase -> 1, UsecaseRev -> 2, Text -> 2, TextRev -> 2, UcField -> 2) {
          saveUseCase(removeNcField(MockUc1.sampleTextOnlyUC), None, projectId)
        }
      }
    }

    describe("Incremental updates") {
      def testUpdateSucceeds(mutate: UseCase => UseCase, expectedTableDiffs: (Table, Int)*) {
        val pid = newProjectId()
        val uc1 = sampleUC
        val cp1 = saveUseCase(uc1, None, pid)
        cp1 should not be (None)
        val uc2 = mutate(uc1)
        val cp2 = assertTableDiffs(expectedTableDiffs: _*) {saveUseCase(uc2, cp1, pid)}
        cp2 should not be (None)
      }

      it("should do nothing when no changes") {
        val pid = newProjectId()
        val uc1 = sampleUC
        val cp1 = saveUseCase(uc1, None, pid)
        cp1 should not be (None)
        assertTableDiffs() {saveUseCase(uc1, cp1, pid)} ==== None
      }

      it("should save a title change") {
        testUpdateSucceeds(ucTitleL.set(_, "zz".validated), UsecaseRev -> 1, UcField -> rels)
      }

      it("should save a text update") {
        testUpdateSucceeds(uc => TF1.lens.set(uc, freeText("jjj")), UsecaseRev -> 1, UcField -> rels, TextRev -> 1)
      }

      it("should save a text removal") {
        testUpdateSucceeds(uc => TF1.lens.set(uc, FreeText.empty), UsecaseRev -> 1, UcField -> rels, TextRev -> 1)
      }

      it("should save a new text") {
        testUpdateSucceeds(uc => TF4.lens.set(uc, freeText("jjj"))
          , UsecaseRev -> 1, UcField -> (rels + 1), Text -> 1, TextRev -> 1)
      }

      it("should save a step text update") {
        testUpdateSucceeds(uc => NCF.updateText(NcSfv.tree(1).id, "qqq")(uc).gimme
          , UsecaseRev -> 1, UcField -> rels, TextRev -> 1)
      }
    }
  }

  describe("Saving then Loading") {
    it("should load in full after saving") {
      // Save first
      val pid = newProjectId()
      val saved = saveUseCase(sampleUC, None, pid).get

      // Then load back (testing manually)
      val loaded = loadRev(saved.rec, pid).uc
      TF1.lens.get(loaded).text ==== "blah"
      TF2.lens.get(loaded).text ==== ""
      TF3.lens.get(loaded).text ==== "hehe"
      NCF.getTextTree(loaded) should matchTree(NcSteps)
      ECF.getTextTree(loaded) should matchTree(EcSteps)
      loaded.header ==== saved.uc.header
    }

    def reverseTopLevelNodes(f: StepField, uc: UseCase): UseCase = {
      def reverse(v: StepFieldValue) = v.copy(tree = StepTree(fixTopLevelIndices(v.tree.nodes.reverse)))
      NCF.lens.mod(reverse, uc).regenerateStepsAndLabels.afterRespondingToChange(MockExistingStepLabelsChanged)
    }

    it("should load in full after multiple updates") {
      val pid = newProjectId()
      var prevSave: UseCaseSaveCheckpoint = forceUcNumber(saveUseCase(sampleUC, None, pid).get, 7)
      def uc = prevSave.uc

      def testUpdate(mutate: UseCase => UseCase, expectedTableDiffs: (Table, Int)*) = {
        val newUc = mutate(prevSave.uc)
        val cpSaveOp = assertTableDiffs(expectedTableDiffs: _*) {saveUseCase(newUc, Some(prevSave), pid)}
        if (expectedTableDiffs.isEmpty) {
          cpSaveOp ==== None
        } else {
          val cpLoad = reload(cpSaveOp.get, pid)
          assertUseCasesMatch(cpLoad.uc, newUc)
          prevSave = cpSaveOp.get
        }
      }

      // Change title
      testUpdate(ucTitleL.set(_, "zzzzzzzzz".validated), UsecaseRev -> 1, UcField -> rels)

      // Change text field
      testUpdate(uc => TF1.lens.set(uc, freeText("jjj")), UsecaseRev -> 1, TextRev -> 1, UcField -> rels)

      // Clear text field
      testUpdate(uc => TF1.lens.set(uc, FreeText.empty), UsecaseRev -> 1, TextRev -> 1, UcField -> rels)

      // Restore text field
      testUpdate(uc => TF1.lens.set(uc, freeText("Back!")), UsecaseRev -> 1, TextRev -> 1, UcField -> rels)

      // Reorder @ L1
      testUpdate(uc => reverseTopLevelNodes(NCF, uc), UsecaseRev -> 1, UcField -> rels)

      // Step text change @ L2
      testUpdate(uc => {
        val stepId = NCF.lens.get(uc).tree(1)(0).id
        ucStepTextInstL.mod(_.update("Roar.")(uc.stepsAndLabels).gimme, (uc, (NCF, stepId)))
      }, UsecaseRev -> 1, TextRev -> 1, UcField -> rels)

      // Add new (empty) step, link from text field
      testUpdate(uc => {
        val uc2 = NCF.addTailStep(uc).gimme
        val newText = "New step is [7.2]"
        val uc3 = TF1.lens.mod(t => t.update(newText)(uc2.stepsAndLabels).gimme, uc2)
        TF1.lens.get(uc3).text ==== newText
        uc3
      }, UsecaseRev -> 1, Text -> 1, TextRev -> 2, UcField -> (rels + 1))

      // Reorder @ L1 (affects linking text field)
      testUpdate(uc => {
        val uc2 = reverseTopLevelNodes(NCF, uc)
        TF1.lens.get(uc2).text ==== "New step is [7.0]"
        uc2
      }, UsecaseRev -> 1, UcField -> (rels + 1))

      // NOP update to text with ref
      testUpdate(uc => TF1.updateText("New step is [7.0]")(uc).getOrElse(uc))
    }

    it("should normalise and de-normalise refs in text") {
      // Save first
      val pid = newProjectId()
      val a = TF1.updateText("Text like [7.0]")(sampleUC).gimme
      val saved = NCF.updateText(NcSfv.tree(0).id, "Step like [7.0.1]")(a).gimme
      val cp = forceUcNumber(saveUseCase(saved, None, pid).get, 7)

      // Confirm stored normalised in DB
      sql"select text from text_rev where text like ${"Text like%"}".as[String].first should not be ("Text like [7.0]")
      sql"select text from text_rev where text like ${"Step like%"}".as[String].first should not be ("Step like [7.0.1]")

      // Then load back
      val loaded = reload(cp, pid).uc
      val ltree = NCF.lens.get(loaded).tree
      TF1.lens.get(loaded).text ==== "Text like [7.0]"
      ucStepTextTextL.get(loaded, (NCF, ltree(0).id)) ==== "Step like [7.0.1]"
    }

    it("should normalise and de-normalise refs in flow") {
      // Save first
      val pid = newProjectId()
      val saved = NCF.updateText(NcSfv.tree(1).id, "Flow like --> [7.0.1]")(sampleUC).gimme
      val cp = forceUcNumber(saveUseCase(saved, None, pid).get, 7)

      // Confirm stored normalised in DB
      sql"select text from text_rev where text like ${"%⬅%"}".as[String].first should not include ("[7.")
      sql"select text from text_rev where text like ${"%➡%"}".as[String].first should not include ("[7.")

      // Then load back
      val loaded = reload(cp, pid).uc
      val ltree = NCF.lens.get(loaded).tree
      ucStepTextTextL.get(loaded, (NCF, ltree(1).id)) ==== "Flow like ➡ [7.0.1]"
      ucStepTextTextL.get(loaded, (NCF, ltree(0)(0).id)) ==== "First ⬅ [7.1]"
    }
  }
}