package com.beardedlogic.usecase
package lib

import net.liftweb.http.CometActor
import org.scalatest.FunSpec
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation
import test.{TestHelpers, TestDatabaseSupport}
import test.NodeUtils._
import TestHelpers._
import field._
import TypeTags._
import model._

class UseCaseCtxTest extends FunSpec with TestDatabaseSupport with TestHelpers {

  implicit def autoTagLocalStepId(s: String) = s.asLocalId

  describe("Loading") {
    it("should load a simple, manually-saved UC") {
      // Create UC
      val uc = db.createInitialValue(DataType.UseCase)
      val uc_id = uc.valueId
      sqlu"INSERT INTO usecase VALUES(${uc_id}, 'ahh', 3, ${Defaults.FieldList.valueId})".execute

      // Create Text FV
      val txt_fk = Defaults.FieldList.fieldKeys.filter(_.fieldKeyType == FieldKeyType.Text).head
      val txt_fv = db.createInitialValue(DataType.FieldValue)
      db.createFieldValue(txt_fv, txt_fk, Some("Hehe!"))
      db.relate_usecase_has_fieldValue(uc, txt_fv)

      // Create course FV
      val c_fk = Defaults.FieldList.fieldKeys.filter(_.fieldKeyType == FieldKeyType.NormalAndAlternateCourses).head
      val c_fv = db.createInitialValue(DataType.FieldValue)
      val s1, s2 = db.createInitialValue(DataType.Step)
      db.createFieldValue(c_fv, c_fk, None)
      db.createStep(s1, "Root")
      db.createStep(s2, "Child")
      db.relate_usecase_has_fieldValue(uc, c_fv)
      db.relate_stepParent_has_step(c_fv, 0, s1)
      db.relate_stepParent_has_step(s1, 0, s2)

      // Load
      val loaded = new UseCaseCtx(null)
      loaded.init // TODO why do i need to call init myself all the time?
      val cp = UseCaseLoader.loadCheckpoint(uc_id, db)
      loaded.restoreCheckpoint(cp.get)

      // Verify
      loaded.title should be("ahh")
      loaded.number should be(3)
      loaded.fields.filter(_.fieldKey == txt_fk).head.asInstanceOf[TextField].value.text should be("Hehe!")
      loaded.ncacField.get.coursesWithText should matchTree(parseStepTree("3.0. Root\n  1. Child"))
    }

    it("should load a manually-saved UC with refs") {
      // Create UC
      val uc = db.createInitialValue(DataType.UseCase)
      val uc_id = uc.valueId
      sqlu"INSERT INTO usecase VALUES(${uc_id}, 'ahh', 3, ${Defaults.FieldList.valueId})".execute

      // Create course FV
      val c_fk = Defaults.FieldList.fieldKeys.filter(_.fieldKeyType == FieldKeyType.NormalAndAlternateCourses).head
      val c_fv = db.createInitialValue(DataType.FieldValue)
      val s1, s2, s3 = db.createInitialValue(DataType.Step)
      db.createFieldValue(c_fv, c_fk, None)
      db.createStep(s1, "Root")
      db.createStep(s2, s"Child [D.${s1.dataId}]")
      db.createStep(s3, s"Other [D.${s2.dataId}]")
      db.relate_usecase_has_fieldValue(uc, c_fv)
      db.relate_stepParent_has_step(c_fv, 0, s1)
      db.relate_stepParent_has_step(c_fv, 1, s3)
      db.relate_stepParent_has_step(s1, 0, s2)

      // Create Text FV
      val txt_fk = Defaults.FieldList.fieldKeys.filter(_.fieldKeyType == FieldKeyType.Text).head
      val txt_fv = db.createInitialValue(DataType.FieldValue)
      db.createFieldValue(txt_fv, txt_fk, Some(s"look at [D.${s2.dataId}] and [D.${s3.dataId}]!"))
      db.relate_usecase_has_fieldValue(uc, txt_fv)

      // Load
      val loaded = new UseCaseCtx(null)
      loaded.init // TODO why do i need to call init myself all the time?
      val cp = UseCaseLoader.loadCheckpoint(uc_id, db)
      loaded.restoreCheckpoint(cp.get)

      // Verify
      loaded.title should be("ahh")
      loaded.number should be(3)
      loaded.fields.filter(_.fieldKey == txt_fk).head.asInstanceOf[TextField].value.text should be("look at [3.0.1] and [3.1]!")
      val nc = loaded.ncacField.get
      nc.test__textFields(nc.courses(0).id).text should be("Root")
      nc.test__textFields(nc.courses(0)(0).id).text should be("Child [3.0]")
      nc.test__textFields(nc.courses(1).id).text should be("Other [3.0.1]")
      //loaded.ncacField.get.coursesWithText should matchTree(parseStepTree("3.0. Root\n  1. Child [3.0]\n3.1. Other [3.0.1]"))
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  // TODO Share sample courses. Create TestData or something.
  lazy val NcSteps = parseStepTree("""
      1.0. I'm the title
        1. First
        2. _
        3. Finally
      1.1. Sweet
                                   """)

  lazy val EcSteps = parseStepTree("""
      1.E.1. EC-1E1
        1. EC-1E1-1
      1.E.2. EC-1E2 """)

  def sampleCtx = {
    val uc = new UseCaseCtx(mock[CometActor])
    uc.title = "YES!"
    uc.textFields(0).value.setTextFromUser("blah")
    uc.textFields(2).value.setTextFromUser("hehe")
    uc.ncacField.get.setCoursesWithText(NcSteps)
    uc.ecField.get.setCoursesWithText(EcSteps)
    uc.init
    uc
  }

  describe("Saving (first-time)") {
    it("should set lastSave (on first save)") {
      val uc = new UseCaseCtx(null)
      uc.init
      uc.lastSave should be('empty)
      uc.save(db)
      uc.lastSave should not be ('empty)
    }

    it("should save when empty") {
      val uc = new UseCaseCtx(null)
      uc.courseFields.foreach(_.courses = Nil)
      assertTableDiffs("usecase" -> 1, "data" -> 1, "value" -> 1) { uc.save(db) }
    }

    it("should save with 2 text fields") {
      val uc = sampleCtx
      uc.courseFields.foreach(_.courses = Nil)
      assertTableDiffs("usecase" -> 1, "data" -> 3, "value" -> 3, "field_value" -> 2, "relation" -> 2) { uc.save(db) }
    }
  }

  describe("Updating") {
    def testUpdate(test: UseCaseCtx => Any, expectUpdate: Boolean = true) {
      val uc = sampleCtx
      uc.save(db)
      uc.lastSave should not be ('empty)
      val lastSave = uc.lastSave
      test(uc)
      uc.lastSave should (if (expectUpdate) (not be (lastSave)) else be(lastSave))
    }

    it("should do nothing when no changes") {
      testUpdate(expectUpdate = false, test = { uc =>
        assertTableDiffs() { uc.save(db) }
      })
    }

    it("should save a title change") {
      testUpdate { uc =>
        uc.title = "zzzzzzzzz"
        assertTableDiffs("usecase" -> 1, "value" -> 1, "relation" -> FVs) { uc.save(db) }
      }
    }

    it("should save a UC-number change") {
      testUpdate { uc =>
        uc.number = 666
        assertTableDiffs("usecase" -> 1, "value" -> 1, "relation" -> FVs) { uc.save(db) }
      }
    }

    it("should save a text update") {
      testUpdate { uc =>
        uc.textFields(0).value.setTextFromUser("jjjjjjjjjj")
        assertTableDiffs("usecase" -> 1, "field_value" -> 1, "value" -> 2, "relation" -> FVs) { uc.save(db) }
      }
    }

    it("should save a text removal") {
      testUpdate { uc =>
        uc.textFields(0).value.setTextFromUser("")
        assertTableDiffs("usecase" -> 1, "value" -> 1, "relation" -> FVsPlus(-1)) { uc.save(db) }
      }
    }

    it("should save a new text") {
      testUpdate { uc =>
        uc.textFields(3).value.setTextFromUser("jjjjjjjjjj")
        assertTableDiffs("usecase" -> 1, "field_value" -> 1, "value" -> 2, "data" -> 1, "relation" -> FVsPlus(1)) { uc.save(db) }
      }
    }

    it("should behave the same on updates after updates") {
      val uc = sampleCtx
      def save = uc.save(db)
      def assertNoSubsequentUpdates = 2.times{ assertTableDiffs() { save } }
      def assertUpdate(expectations: Seq[(String, Int)]) = { assertTableDiffs(expectations: _*)(save); assertNoSubsequentUpdates }
      save; assertNoSubsequentUpdates
      simulateMultipleUpdates(uc, assertUpdate)
    }
  }

  def fixTopLevelIndices(nodes: List[StepNode]): List[StepNode] = for ((n, i) <- nodes.zipWithIndex) yield n.copy(labelIndex = i)
  def FVs = 4 // 2 text fields + NC/AC + EC
  def FVsPlus(plus: Int) = FVs + plus

  def simulateMultipleUpdates(uc: UseCaseCtx, testUpdateFn: Seq[(String, Int)] => Any) = {
    def testUpdate(expectations: (String, Int)*) = testUpdateFn(expectations)

    uc.msgCentre.enabled = true

    // Change title
    uc.title = "zzzzzzzzz"
    testUpdate("usecase" -> 1, "value" -> 1, "relation" -> FVs)

    // Change text field
    uc.textFields(0).value.setTextFromUser("jjjjjjjjjj")
    testUpdate("usecase" -> 1, "field_value" -> 1, "value" -> 2, "relation" -> FVs)

    // Clear text field
    uc.textFields(0).value.setTextFromUser("")
    testUpdate("usecase" -> 1, "value" -> 1, "relation" -> FVsPlus(-1))

    // Restore text field
    uc.textFields(0).value.setTextFromUser("Back!")
    // TODO A new FV is created when text is deleted and restored. Should it not maintain the FV audit trail?
    testUpdate("usecase" -> 1, "field_value" -> 1, "value" -> 2, "data" -> 1, "relation" -> FVs)

    // Reorder @ L1
    val ncac = uc.ncacField.get
    ncac.courses = fixTopLevelIndices(ncac.courses.reverse)
    testUpdate("usecase" -> 1, "field_value" -> 1, "value" -> 2, "relation" -> FVsPlus(ncac.courses.size))

    // Step text change @ L2
    ncac.test__textFields(ncac.courses(1)(0).id).setTextFromUser("Roar.")
    // 1.0.1: New step + value -- S:1 V:1
    // 1.0: New step + value   -- S:1 V:1
    // 1.0 has 3 children      --         R:3
    // FV has 2 steps          --     V:1 R:2
    // Usecase + value         --     V:1
    testUpdate("usecase" -> 1, "field_value" -> 1, "step" -> 2, "value" -> 4, "relation" -> FVsPlus(5))

    // Ref to new (empty) step
    ncac.addTailStep()
    ncac.courses.size should be(3)
    uc.textFields(0).value.setTextFromUser("New step is [1.2]")
    // 1.2: New step  -- S:1 V:1 D:1
    // Text update    --     V:1     FV:1
    // FV has 3 steps --     V:1     FV:1 R:3
    // UC             --     V:1          R:FVs
    testUpdate("usecase" -> 1, "field_value" -> 2, "step" -> 1, "value" -> 4, "data" -> 1, "relation" -> FVsPlus(3))

    // Reorder to step referred to by others
    ncac.courses = fixTopLevelIndices(ncac.courses.reverse)
    eventually(uc.textFields(0).value.text should be("New step is [1.0]"))
    testUpdate("usecase" -> 1, "field_value" -> 1, "value" -> 2, "relation" -> FVsPlus(ncac.courses.size))
  }

  describe("Saving then Loading") {
    it("should load in full after saving") {
      // Save first
      val saved = sampleCtx
      val valueRows = countRowsIn("value")
      saved.save(db)
      (countRowsIn("value") - valueRows) should be > 10

      // Then load back (testing manually)
      val loaded = loadAndAssertShallow(saved)
      loaded.textFields(0).value.text should be("blah")
      loaded.textFields(1).value.text should be("")
      loaded.textFields(2).value.text should be("hehe")
      loaded.ncacField.get.coursesWithText should matchTree(NcSteps)
      loaded.ecField.get.coursesWithText should matchTree(EcSteps)

      // Quick sanity check on loadAndAssertDeep
      loadAndAssertDeep(saved)
    }

    it("should load in full after multiple updates") {
      val uc = sampleCtx
      def testFn(expectations: Seq[(String, Int)]) = {
        assertTableDiffs(expectations: _*)(uc.save(db))
        loadAndAssertDeep(uc)
      }
      uc.save(db)
      simulateMultipleUpdates(uc, testFn)
    }

    it("should normalise and de-normalise refs in text") {
      // Save first
      val saved = sampleCtx
      saved.textFields(0).value.setTextFromUser("Text like [1.0]")
      saved.ncacField.get.test__textFields.values.head.setTextFromUser("Step like [1.0.1]")
      saved.save(db)
      saved.lastSave.get.fieldStates.toString should include("Text like")
      saved.lastSave.get.fieldStates.toString should include("Step like")

      // Confirm stored normalised in DB
      sql"select text from field_value where text like ${"Text like%"}".as[String].first should not be("Text like [1.0]")
      sql"select text from step where text like ${"Step like%"}".as[String].first should not be("Step like [1.0.1]")

      // Then load back
      val loaded = loadAndAssertShallow(saved)
      loaded.textFields(0).value.text should be("Text like [1.0]")
      loaded.ecField.get.coursesWithText should matchTree(EcSteps)
      val stepTexts = loaded.ncacField.get.test__textFields.values.map(_.text)
      stepTexts.filter(_.startsWith("Step like")).headOption should be(Some("Step like [1.0.1]"))
    }

    it("should normalise and de-normalise refs in flow") {
      // Save first
      val saved = sampleCtx
      saved.msgCentre.enabled = true
      saved.ncacField.get.test__textFields.values.filter(_.text == "Sweet").head.setTextFromUser("Flow like --> [1.0.1]")
      saved.save(db)
      saved.lastSave.get.fieldStates.toString should include("➡")
      saved.lastSave.get.fieldStates.toString should include("⬅")

      // Confirm stored normalised in DB
      sql"select text from step where text like ${"%⬅%"}".as[String].first should not include("[1.")
      sql"select text from step where text like ${"%➡%"}".as[String].first should not include("[1.")

      // Then load back
      val loaded = loadAndAssertShallow(saved)
      loaded.ecField.get.coursesWithText should matchTree(EcSteps)
      val stepTexts = loaded.ncacField.get.test__textFields.values.map(_.text)
      stepTexts.filter(_.contains("➡")).headOption should be(Some("Flow like ➡ [1.0.1]"))
      stepTexts.filter(_.contains("⬅")).headOption should be(Some("First ⬅ [1.1]"))
    }
  }

  def valueIdOf(uc: UseCaseCtx) = uc.lastSave.get.uc.valueId

  def load(target: UseCaseCtx, valueId: Long) {
    val checkpoint = UseCaseLoader.loadCheckpoint(valueId, db)
    checkpoint should not be (None)
    target.restoreCheckpoint(checkpoint.get)
  }

  def loadAndAssertShallow(saved: UseCaseCtx) = {
    val loaded = new UseCaseCtx(null)
    load(loaded, valueIdOf(saved))
    loaded.title should be(saved.title)
    loaded.number should be(saved.number)
    loaded
  }

  def loadAndAssertDeep(saved: UseCaseCtx) = {
    val loaded = loadAndAssertShallow(saved)
    for ((s,l) <- saved.textFields.zip(loaded.textFields)) l.value.text should be(s.value.text)
    for ((s,l) <- saved.courseFields.zip(loaded.courseFields)) l.coursesWithText should matchTree(s.coursesWithText)
    loaded
  }
}
