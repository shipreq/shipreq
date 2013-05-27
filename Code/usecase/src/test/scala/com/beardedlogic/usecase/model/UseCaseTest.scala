package com.beardedlogic.usecase
package model

import net.liftweb.util.Helpers._
import org.scalatest.FunSpec
import com.beardedlogic.usecase.test.{TestHelpers, TestDatabaseSupport}
import lib.UCEditorState
import lib.field._
import lib.StepTree.{Step => Step2, _}

class UseCaseTest extends FunSpec with TestDatabaseSupport with TestHelpers {

  it("should save when empty") {
    val uce = new UCEditorState(1, null)
    uce.courseFields.foreach(_.courses = Nil)
    assertTableDiffs("usecase" -> 1, "data" -> 1, "value" -> 1) {
      db.createInitialUseCase(uce)
    }
  }

  it("should save with 2 text fields") {
    val uce = new UCEditorState(1, null)
    uce.courseFields.foreach(_.courses = Nil)
    val textFields = uce.fields.collect { case f: TextField => f }
    textFields.take(2).foreach(_.value.setTextFromUser("blah"))
    assertTableDiffs("usecase" -> 1, "data" -> 3, "value" -> 3, "field_value" -> 2, "relation" -> 2) {
      db.createInitialUseCase(uce)
    }
  }

  it("should save and load in full") {
    // TODO Share sample courses. Create TestData or something.
    val ncSteps =
      StepNode(nextFuncName, 0, Some("2."), 0, Step2("I'm the title"), (
        new StepNode(nextFuncName, 1, 1, Step2("First")) ::
          new StepNode(nextFuncName, 1, 2, NewStep) ::
          new StepNode(nextFuncName, 1, 3, Step2("Finally"), (
            new StepNode(nextFuncName, 2, 1, Step2("Sweet")) :: Nil
            )) :: Nil
        )) :: Nil
    val ecSteps =
      StepNode(nextFuncName, 0, Some("2.E."), 1, Step2("EC 1E1"), List(new StepNode(nextFuncName, 1, 1, Step2("EC 1E11")))) ::
        StepNode(nextFuncName, 0, Some("2.E."), 2, Step2("EC 1E2"), Nil) ::
        Nil

    val uce = new UCEditorState(2, null)
    uce.title = "YES!"
    uce.textFields(0).value.setTextFromUser("blah")
    uce.textFields(2).value.setTextFromUser("hehe")
    uce.ncacField.get.courses = ncSteps
    uce.ecField.get.courses = ecSteps

    val valueRows = countRowsIn("value")
    val saved = db.createInitialUseCase(uce)
    (countRowsIn("value") - valueRows) should be > 10

    val loaded = UCEditorState.load(saved.valueId, null, db).get
    loaded.title should be(uce.title)
    loaded.ucNumber should be(uce.ucNumber)
    loaded.textFields(0).value.text should be("blah")
    loaded.textFields(1).value.text should be("")
    loaded.textFields(2).value.text should be("hehe")
    loaded.ncacField.get.courses should matchTree(ncSteps)
    loaded.ecField.get.courses should matchTree(ecSteps)
  }
}
