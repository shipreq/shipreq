package shipreq.webapp
package feature.uc
package persist

import org.scalatest.FunSpec
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation

import lib.Locks
import lib.Types._
import change.UseCaseUpdater
import db.UseCaseHeader
import field._
import step.StepTree
import test.{LoadedTestData, TestDatabaseSupport, TestHelpers}
import text.{StepTextUpdater, FreeText}
import Lenses._
import UseCasePersistence._

class UseCasePersistenceTest extends FunSpec with TestDatabaseSupport with TestHelpers with LoadedTestData {
  import Tables._
  import MockUc1._
  val rels = 2 + 5 + 3 // 2 text fields + 5 NC steps + 3 EC steps

  implicit def autoCtx(sl: StepAndLabelBiMap) = UcParsingCtx.Empty.copy(stepsAndLabels = sl)
  implicit def ucTu(uc: UseCase) = UseCaseUpdater(uc, UseCaseRelations.Empty)

  def loadRev(revId: UseCaseRevId, projectId: ProjectId): UseCaseSaveCheckpoint = {
    val rev = dao.findUseCaseRev(revId).get
    Locks.UseCaseNumbers.read(projectId)(load(rev).run(dao, _))
  }

  def reload(cp: UseCaseSaveCheckpoint, projectId: ProjectId) = loadRev(cp.rec, projectId)

  def createInitialTextRev(ucIdentId: UseCaseIdentId, fkId: FieldKeyId, text: String) =
    dao.createTextRev(dao.createTextIdent(ucIdentId, fkId), 1, NormalisedText(text))

  describe("Loading") {
    it("should set NC.0 to the title for new UCs") {
      val pid = newProjectId()
      val x = createUseCaseIdentAndRev1(pid, UseCaseHeader("Hello"))
      val y = loadRev(x, pid)
      val sfv = NCF.lens.get(y.uc)
      sfv.textmap(sfv.tree.head.id).text ==== "Hello."
    }

    it("should load a simple, manually-saved UC") {
      // Create UC
      val pid = newProjectId()
      val ucIdent = dao.createUseCaseIdentWithForcedNumber(pid, UseCaseNumber(3))
      val ucRev = dao.createUseCaseRev(ucIdent, 1, UseCaseHeader("ahh"))

      // Create Text FV
      val txtRev = createInitialTextRev(ucIdent, TF1, "Hehe")
      dao.linkUcToText(ucRev, txtRev)

      // Create course FV
      val s1 = createInitialTextRev(ucIdent, NCF, "Root")
      val s2 = createInitialTextRev(ucIdent, NCF, "Child")
      dao.linkUcToStep(ucRev, StepLabel("3.0"), 0, None, s1)
      dao.linkUcToStep(ucRev, StepLabel("3.0.1"), 0, Some(s1.id), s2)

      // Load
      val loaded = loadRev(ucRev, pid).uc

      // Verify
      loaded.header ==== UseCaseHeader("ahh")
      TF1(loaded.fieldValues).text ==== "Hehe"
      assertStepTree(loaded, NCF, "3.0. Root\n  1. Child")
    }

    it("should load a manually-saved UC with refs") {
      // Create UC
      val pid = newProjectId()
      val ucIdent = dao.createUseCaseIdentWithForcedNumber(pid, UseCaseNumber(3))
      val ucRev = dao.createUseCaseRev(ucIdent, 1, UseCaseHeader("ahh"))

      // Create course FV
      val s1 = createInitialTextRev(ucIdent, NCF, "Root")
      val s2 = createInitialTextRev(ucIdent, NCF, s"Child [D.${s1.identId.value}]")
      val s3 = createInitialTextRev(ucIdent, NCF, s"Other [D.${s2.identId.value}]")
      dao.linkUcToStep(ucRev, StepLabel("3.0"), 0, None, s1)
      dao.linkUcToStep(ucRev, StepLabel("3.0.1"), 0, Some(s1.id), s2)
      dao.linkUcToStep(ucRev, StepLabel("3.1"), 1, None, s3)

      // Create Text FV
      val txtRev = createInitialTextRev(ucIdent, TF3, s"look at [D.${s2.identId.value}] and [D.${s3.identId.value}]!")
      dao.linkUcToText(ucRev, txtRev)

      // Load
      val loaded = loadRev(ucRev, pid).uc

      // Verify
      loaded.header ==== UseCaseHeader("ahh")
      TF3(loaded.fieldValues).text ==== "look at [3.0.1] and [3.1]!"
      assertStepTree(loaded, NCF, "3.0. Root\n  1. Child [3.0]\n3.1. Other [3.0.1]")
    }

    it("should realise UC refs") {
      // Create UCs
      val pid = newProjectId()
      createUseCaseIdentAndRev1(pid, UseCaseHeader("First!"))
      val r = createUseCaseIdentAndRev1(pid, UseCaseHeader("SECOND"))
      dao.linkUcToText(r, createInitialTextRev(r, TF1, "I LIKE [UC-1] AND [UC-2]"))

      // Load and verify
      val loaded = loadRev(r, pid).uc
      TF1(loaded.fieldValues).text ==== "I LIKE [UC-1: First!] AND [UC-2: SECOND]"
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
        testUpdateSucceeds(ucTitleL.set(_, "zz"), UsecaseRev -> 1, UcField -> rels)
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
      testUpdate(ucTitleL.set(_, "zzzzzzzzz"), UsecaseRev -> 1, UcField -> rels)

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
        ucStepTextInstL.mod(t => new StepTextUpdater(NCF, stepId).updateAndGet(t, "Roar.")(uc.stepsAndLabels), (uc, (NCF, stepId)))
      }, UsecaseRev -> 1, TextRev -> 1, UcField -> rels)

      // Add new (empty) step, link from text field
      testUpdate(uc => {
        val uc2 = NCF.addTailStep(uc).gimme
        val newText = "New step is [7.2]"
        val uc3 = TF1.lens.mod(t => TF1.changeResponder.updateAndGet(t, newText)(uc2.stepsAndLabels), uc2)
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
      testUpdate(uc => TF1.updateText("New step is [7.0]")(uc).getValueOrElse(uc))
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
