package com.beardedlogic.usecase
package model

import net.liftweb.util.Helpers._
import org.scalatest.FunSpec
import com.beardedlogic.usecase.test.{TestHelpers, TestDatabaseSupport}
import lib.UseCaseCtx
import lib.field._
import lib.StepTree.{Step => Step2, _}

class UseCaseTest extends FunSpec with TestDatabaseSupport with TestHelpers {

  // TODO Share sample courses. Create TestData or something.
  val NcSteps =
   StepNode(nextFuncName, 0, 0, Step2("I'm the title"), (
        StepNode(nextFuncName, 1, 1, Step2("First")) ::
          StepNode(nextFuncName, 1, 2, NewStep) ::
          StepNode(nextFuncName, 1, 3, Step2("Finally"), (
            StepNode(nextFuncName, 2, 1, Step2("Sweet")) :: Nil
            )) :: Nil
        )) :: Nil

  val EcSteps =
    StepNode(nextFuncName, 0, 1, Step2("EC 1E1"), List(StepNode(nextFuncName, 1, 1, Step2("EC 1E11")))) ::
        StepNode(nextFuncName, 0, 2, Step2("EC 1E2")) ::
        Nil

  def sampleCtx = {
    val ctx = new UseCaseCtx(null)
    ctx.title = "YES!"
    ctx.textFields(0).value.setTextFromUser("blah")
    ctx.textFields(2).value.setTextFromUser("hehe")
    ctx.ncacField.get.courses = NcSteps
    ctx.ecField.get.courses = EcSteps
    ctx
  }

  describe("Saving") {
    it("should set the data & lastSave values (on first save)") {
      val uce = new UseCaseCtx(null)
      uce.dataRec should be ('empty)
      uce.lastSave should be ('empty)
      uce.save(db)
      uce.dataRec should not be ('empty)
      uce.lastSave should not be ('empty)
    }

    it("should save when empty") {
      val uce = new UseCaseCtx(null)
      uce.courseFields.foreach(_.courses = Nil)
      assertTableDiffs("usecase" -> 1, "data" -> 1, "value" -> 1) {
        uce.save(db)
      }
    }

    it("should save with 2 text fields") {
      val uce = new UseCaseCtx(null)
      uce.courseFields.foreach(_.courses = Nil)
      val textFields = uce.fields.collect { case f: TextField => f }
      textFields.take(2).foreach(_.value.setTextFromUser("blah"))
      assertTableDiffs("usecase" -> 1, "data" -> 3, "value" -> 3, "field_value" -> 2, "relation" -> 2) {
        uce.save(db)
      }
    }

    ignore("should do nothing when no changes") {
//      val uce = sampleCtx
//      db.createInitialUseCase(sampleCtx)
//      assertTableDiffs() { db.createInitialUseCase(sampleCtx) }
    }

    // TODO save when 1 text change

    // TODO save when 1 step text change

    // TODO save when step order change
  }

  describe("Loading") {
    it("should load in full after saving") {
      // Save first
      val saved = sampleCtx
      val valueRows = countRowsIn("value")
      saved.save(db)
      (countRowsIn("value") - valueRows) should be > 10
      val valueId = saved.lastSave.get._1.valueId

      // Then load back
      val loaded = new UseCaseCtx(null)
      loaded.load(valueId, db)
      loaded.title should be(saved.title)
      loaded.number should be(saved.number)
      loaded.textFields(0).value.text should be("blah")
      loaded.textFields(1).value.text should be("")
      loaded.textFields(2).value.text should be("hehe")
      loaded.ncacField.get.courses should matchTree(NcSteps)
      loaded.ecField.get.courses should matchTree(EcSteps)
      // TODO fails due to IDs -- loaded.currentState should be(saved.currentState)
    }
  }
}


