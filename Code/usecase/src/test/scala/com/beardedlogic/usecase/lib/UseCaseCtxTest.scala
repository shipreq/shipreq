package com.beardedlogic.usecase
package lib

import org.scalatest.FunSpec
import test.{TestHelpers, TestDatabaseSupport}
import field._
import model._
import NodeUtils._
import StepTree.{Step => Step2, _}
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation

class UseCaseCtxTest extends FunSpec with TestDatabaseSupport with TestHelpers {

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
      loaded.ncacField.get.courses should matchTree(parseStepTree("3.0. Root\n  1. Child"))
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

  /*
  val Steps1A =
    StepNode("1E1", 0, 1, Step("EC 1E1"), List(StepNode("1E1.1", 1, 1, Step("EC 1E11")))) ::
      StepNode("1E2", 0, 2, Step("EC 1E2")) ::
      Nil

  val Steps1B =
    StepNode("v10", 0, 1, Step("EC 1E1"), List(StepNode("v12", 1, 1, Step("EC 1E11")))) ::
      StepNode("v11", 0, 2, Step("EC 1E2")) ::
      Nil

  val Steps2_TextChange =
    StepNode("v10", 0, 1, Step("EC 1E1"), List(StepNode("v12", 1, 1, Step("i differ")))) ::
      StepNode("v11", 0, 2, Step("EC 1E2")) ::
      Nil

  val Steps2_OrderChange =
    StepNode("v11", 0, 2, Step("EC 1E2")) ::
      StepNode("v10", 0, 1, Step("EC 1E1"), List(StepNode("v12", 1, 1, Step("EC 1E11")))) ::
      Nil

  def fs(courses: List[StepNode]) = Map(FieldKey(5, FieldKeyType.NormalAndAlternateCourses, None) -> courses)
  def ucs(number: Short, title: String, courses: List[StepNode]) = UseCaseState(number, title, fs(courses))
*/
  /*
  describe("UseCaseState comparison") {
    it("should match when content is the same") {
      val a = ucs(2, "Hehe", Steps1B)
      val b = ucs(2, "Hehe", Steps1B.map(_.copy()))
      val c = a.copy(number = 2, title = "Hehe")
      a.sameContentAs(a) should be(true)
      a.sameContentAs(b) should be(true)
      a.sameContentAs(c) should be(true)
    }

    it("should differ when simple content changes") {
      val a = ucs(2, "Hehe", Steps1B)
      val b = a.copy(number = 3)
      val c = a.copy(title = "What")
      a.sameContentAs(b) should be(false)
      a.sameContentAs(c) should be(false)
    }

    it("should differ when course content changes") {
      val a = ucs(2, "Hehe", Steps1B)
      val b = a.copy(number = 3)
      val c = a.copy(title = "What")
      a.sameContentAs(b) should be(false)
      a.sameContentAs(c) should be(false)
    }
  }
  */
}