package com.beardedlogic.usecase
package lib

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

  implicit def autoTagLocalStepId(s: String) = s.asLocalStepId

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
      // loaded.ncacField.get.courses should matchTree(parseStepTree("3.0. Root\n  1. Child [3.0]\n3.1. Other [3.0.1]"))
      // TODO courses.step.text contains normalised refs. Good/bad?
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
    val uc = new UseCaseCtx(null)
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

    def FVs = 4
    def FVsPlus(plus: Int) = FVs + plus

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
      uc.save(db)
      assertTableDiffs() { uc.save(db) }
      assertTableDiffs() { uc.save(db) }

      uc.title = "zzzzzzzzz"
      assertTableDiffs("usecase" -> 1, "value" -> 1, "relation" -> FVs) { uc.save(db) }

      assertTableDiffs() { uc.save(db) }
      assertTableDiffs() { uc.save(db) }

      uc.textFields(0).value.setTextFromUser("jjjjjjjjjj")
      assertTableDiffs("usecase" -> 1, "field_value" -> 1, "value" -> 2, "relation" -> FVs) { uc.save(db) }

      assertTableDiffs() { uc.save(db) }
      assertTableDiffs() { uc.save(db) }

      uc.textFields(0).value.setTextFromUser("")
      assertTableDiffs("usecase" -> 1, "value" -> 1, "relation" -> FVsPlus(-1)) { uc.save(db) }

      assertTableDiffs() { uc.save(db) }
      assertTableDiffs() { uc.save(db) }
    }

    // TODO save when 1 step text change

    // TODO save when step order change
  }

  describe("Saving then Loading") {
    it("should load in full after saving") {
      // Save first
      val saved = sampleCtx
      val valueRows = countRowsIn("value")
      saved.save(db)
      (countRowsIn("value") - valueRows) should be > 10
      val valueId = saved.lastSave.get.uc.valueId

      // Then load back
      val loaded = new UseCaseCtx(null)
      load(loaded, valueId)
      loaded.title should be(saved.title)
      loaded.number should be(saved.number)
      loaded.textFields(0).value.text should be("blah")
      loaded.textFields(1).value.text should be("")
      loaded.textFields(2).value.text should be("hehe")
      loaded.ncacField.get.coursesWithText should matchTree(NcSteps)
      loaded.ecField.get.coursesWithText should matchTree(EcSteps)
    }

    it("should normalise and de-normalise refs") {
      // Save first
      val saved = sampleCtx
      saved.textFields(0).value.setTextFromUser("Text like [1.0]")
      saved.ncacField.get.test__textFields.values.head.setTextFromUser("Step like [1.0.1]")
      saved.save(db)
      saved.lastSave.get.fieldStates.toString should include("Text like")
      saved.lastSave.get.fieldStates.toString should include("Step like")
      val valueId = saved.lastSave.get.uc.valueId

      // Confirm stored normalised in DB
      sql"select text from field_value where text like ${"Text like%"}".as[String].first should not be("Text like [1.0]")
      sql"select text from step where text like ${"Step like%"}".as[String].first should not be("Step like [1.0.1]")

      // Then load back
      val loaded = new UseCaseCtx(null)
      load(loaded, valueId)
      loaded.title should be(saved.title)
      loaded.number should be(saved.number)
      loaded.textFields(0).value.text should be("Text like [1.0]")
      loaded.ecField.get.coursesWithText should matchTree(EcSteps)
      val stepTexts = loaded.ncacField.get.test__textFields.values.map(_.text)
      stepTexts.filter(_.startsWith("Step like")).headOption should be(Some("Step like [1.0.1]"))
    }
  }

  def load(ucCtx: UseCaseCtx, valueId: Long) {
    val checkpoint = UseCaseLoader.loadCheckpoint(valueId, db)
    checkpoint should not be (None)
    ucCtx.restoreCheckpoint(checkpoint.get)
  }
}
