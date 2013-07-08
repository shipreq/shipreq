package com.beardedlogic.usecase
package snippet

import com.beardedlogic.usecase.lib.{ExternalId, Defaults}
import com.beardedlogic.usecase.model.{UseCase, UseCaseSummary}
import com.beardedlogic.usecase.test.TestDatabaseSupport
import com.beardedlogic.usecase.util.{ErrorMessages, JavaScriptReaction, NoReaction}
import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks

class UseCaseIndexSnippetTest extends FunSpec with TestDatabaseSupport with PropertyChecks {

  describe("#createNewUseCase") {
    def createNewUseCase: UseCaseSummary = assertTableDiffs('data -> 1, 'value -> 1, 'usecase -> 1) {
      UseCaseIndex.createNewUseCase(NoReaction, db)
    }

    it("should create the first as \"1. Untitled\"") {
      truncate('usecase)
      val uc = createNewUseCase
      uc.number should be(1)
      uc.title should be("Untitled")
    }

    // TODO New-UC has GLOBAL scope.

    it("should create the second as \"2. Untitled\"") {
      truncate('usecase)
      createNewUseCase
      val uc = createNewUseCase
      uc.number should be(2)
      uc.title should be("Untitled")
    }
  }

  // TODO Test ucCtx.save corrects UC titles too
  describe("#updateUseCaseHeader") {
    def assertUpdateTriggered(js: JavaScriptReaction) {
      js.result.toString should (include(UseCaseIndex.UseCaseUpdated.triggerName) and include("trigger"))
    }

    def assertUpdateNotTriggered(js: JavaScriptReaction) {
      js.result.toString should not include ("trigger")
    }

    def newUc = db.createInitialUseCase(Defaults.Title, Defaults.FieldList.get)

    def params(dataId: Long, valueId: Long, newTitle: String) =
      Map("dataEid" -> ExternalId(dataId), "valueEid" -> ExternalId(valueId), "title" -> newTitle)

    def test(params: Map[String, String]) = {
      val js = new JavaScriptReaction
      val r = withSessionParams(params) {
        UseCaseIndex.updateUseCaseHeader(js.reactor)
      }
      (r, js)
    }

    def testSuccess(newTitle: String, expectedTitleAfterSave: String): UseCaseSummary =
      testSuccess2(newUc, newTitle, expectedTitleAfterSave)

    def testSuccess2(uc1: UseCase, newTitle: String, expectedTitleAfterSave: String): UseCaseSummary = {
      val (r, js) = test(params(uc1.dataId, uc1.valueId, newTitle))
      r should be('defined)
      val uc2 = r.openOrThrowException("required")
      assertJsErrorNotice(js, None)
      assertUpdateTriggered(js)
      uc2.number should equal(uc1.number)
      uc2.title should equal(expectedTitleAfterSave)
      db.findAllUseCaseSummaries() should contain(uc2)
      uc2
    }

    def testFailure(uc: UseCase, errorMsgFrag: String, params: Map[String, String]) {
      val (r, js) = assertTableDiffs()(test(params))
      r should be('empty)
      assertJsErrorNotice(js, Some(errorMsgFrag))
      assertUpdateNotTriggered(js)
      db.findUseCase(uc.valueId) should be(Some(uc))
    }

    it("should update new new UC") {
      testSuccess("great", "great")
    }

    it("should correct invalid titles") {
      val examples = Table(("INPUT", "OUTPUT")
        , ("   omg   ", "omg")
        , ("what     about", "what about")
        , ("what\tabout", "what about")
        , ("\tgreat  work\n", "great work")
        , ("", Defaults.Title) // NOP actually
        , ("    ", Defaults.Title) // NOP actually
      )
      forAll(examples)(testSuccess(_, _))
    }

    it("should appear to update when no change") {
      val uc1 = newUc
      val uc2s = testSuccess2(uc1, "hello", "hello")
      val uc2 = db.findUseCase(uc2s.valueId).get
      assertTableDiffs(){ testSuccess2(uc2, uc2.title, uc2.title) }
      db.findAllUseCaseSummaries() should contain(uc2s)
    }

    it("should reject invalid input data") {
      val uc = newUc
      testFailure(uc, "not found", params(uc.dataId, 987654321, "hell0"))
      testFailure(uc, "not found", params(98732156, uc.valueId, "hell0"))
      testFailure(uc, ErrorMessages.BadRequest, params(uc.dataId, uc.valueId, "") - "title")
    }

    it("should reject updates when UC rev not latest") {
      val uc = newUc
      val uc1 = db.updateUseCaseHeader(uc.copy(title="New Title!")).dataOpt.get // direct update (same valueId)
      val uc2 = db.updateUseCaseHeader(uc1.copy(title="Newer title")).dataOpt.get // audited update
      uc2.valueId should not be(uc.valueId)
      testFailure(uc2, ErrorMessages.StaleDataSubmitted, params(uc.dataId, uc.valueId, "zxcvz"))
    }
  }
}
