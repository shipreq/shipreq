package com.beardedlogic.usecase
package snippet

import org.scalatest.FunSpec
import test.{LiveTestHelpers, LiveTest}
import app.AppSiteMap.Urls
import lib.msg.NoReaction
import LiveTestHelpers._

class UCEditorSnippetTest extends FunSpec with LiveTest {

  describe("GET") {
    it("should 200 with valid ID") {
      val ucs = UseCaseIndex.createNewUseCase(NoReaction, db)
      var uc2 = db.updateUseCaseHeader(db.findUseCase(ucs.valueId).get.copy(title = "OMGBRU")).dataOpt.get
      val r = get(Urls.viewUseCase(uc2)) ! 200
      r.responseText should include("OMGBRU")
    }

    it("should 404 when ID not found") {
      get(Urls.viewUseCase("123456")) ! 404
    }

    it("should 404 with invalid ID") {
      get(Urls.viewUseCase("__")) ! 404
    }
  }
}
