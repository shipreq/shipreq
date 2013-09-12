package com.beardedlogic.usecase
package snippet

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks
import lib.{ExternalId, Defaults}
import lib.Types._
import db.{UseCaseRev, UseCaseSummary}
import test.TestDatabaseSupport
import util.ErrorMessages
import net.liftweb.http.js.JsCmd

class UseCaseIndexSnippetTest extends FunSpec with TestDatabaseSupport with PropertyChecks {
  import Tables._

  describe("#createNewUseCase") {
    def createNewUseCase: UseCaseSummary = assertTableDiffs(Usecase -> 1, UsecaseRev -> 1) {
      UseCaseIndex.create()
    }

    it("should create the first as \"1. Untitled\"") {
      truncate(Usecase)
      val uc = createNewUseCase
      uc.number should be(1)
      uc.title should be("Untitled")
    }

    // TODO New-UC has GLOBAL scope.

    it("should create the second as \"2. Untitled\"") {
      truncate(Usecase)
      createNewUseCase
      val uc = createNewUseCase
      uc.number should be(2)
      uc.title should be("Untitled")
    }
  }

  // TODO Test ucCtx.save corrects UC titles too

  describe("#update") {
    def assertUpdateTriggered(js: JsCmd) {
      js.toJsCmd should (include(UseCaseIndex.TriggerUpdate.triggerName) and include("trigger"))
    }

    def assertUpdateNotTriggered(js: JsCmd) {
      js.toJsCmd should not include ("trigger")
    }

    def assertSummaryInAll(x: UseCaseSummary): Unit =
      db.findAllUseCaseSummaries().map(ignoreTimestamp) should contain(ignoreTimestamp(x))

    def ignoreTimestamp(x: UseCaseSummary) = x.copy(updatedAt = "IGNORED")

    def newUc = db.createInitialUseCase(Defaults.Title)

    def params(id: UseCaseIdentId, newTitle: String) =
      Map("eid" -> ExternalId.UseCase(id), "title" -> newTitle)

    def test(params: Map[String, String]) = {
      withSessionParams(params) {
        val m = UseCaseIndex.update
        val js = UseCaseIndex.onUpdate(m)
        (m, js)
      }
    }

    def testSuccess(newTitle: String, expectedTitleAfterSave: String): UseCaseSummary =
      testSuccess2(newUc, newTitle, expectedTitleAfterSave)

    def testSuccess2(uc1: UseCaseRev, newTitle: String, expectedTitleAfterSave: String): UseCaseSummary = {
      val (r, js) = test(params(uc1, newTitle))
      r should be('defined)
      val uc2 = r.openOrThrowException("required")
      assertJsErrorNotice(js, None)
      assertUpdateTriggered(js)
      uc2.number should equal(uc1.header.number)
      uc2.title should equal(expectedTitleAfterSave)
      assertSummaryInAll(uc2)
      uc2
    }

    def testFailure(uc: UseCaseRev, errorMsgFrag: String, params: Map[String, String]) {
      val (r, js) = assertTableDiffs()(test(params))
      r should be('empty)
      assertJsErrorNotice(js, Some(errorMsgFrag))
      assertUpdateNotTriggered(js)
      db.findUseCase(uc.id) should be(Some(uc))
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
      val uc2 = db.findLatestUseCase(uc2s.parseId.get).get
      assertTableDiffs(){ testSuccess2(uc2, uc2.header.title, uc2.header.title) }
      assertSummaryInAll(uc2s)
    }

    it("should reject invalid input data") {
      val uc = newUc
      testFailure(uc, "not found", params(98732156.tag[UseCaseIdentId], "hell0"))
      testFailure(uc, ErrorMessages.BadRequest, params(uc, "") - "title")
    }

    //it("should reject updates when UC rev not latest") {
    //  val uc = newUc
    //  val uc1 = db.updateUseCaseHeader(uc.identId, _.copy(title = "New Title!")).dataOpt.get // direct update (same valueId)
    //  val uc2 = db.updateUseCaseHeader(uc1.identId, _.copy(title = "Newer title")).dataOpt.get // audited update
    //  uc2.id should not be(uc.id)
    //  testFailure(uc2, ErrorMessages.StaleDataSubmitted, params(uc.identId, uc.id, "zxcvz"))
    //}
  }
}
