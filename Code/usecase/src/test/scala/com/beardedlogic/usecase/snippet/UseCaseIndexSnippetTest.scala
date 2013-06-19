package com.beardedlogic.usecase
package snippet

import org.scalatest.FunSpec
import lib.msg.NoReaction
import model.UseCaseSummary
import test.TestDatabaseSupport

class UseCaseIndexSnippetTest extends FunSpec with TestDatabaseSupport {

  describe("createNewUseCase()") {
    def createNewUseCase: UseCaseSummary = assertTableDiffs('data -> 1, 'value -> 1, 'usecase -> 1) {
      UseCaseIndex.createNewUseCase(NoReaction, db)
    }

    it("should create the first as '1. Untitled'") {
      val uc = createNewUseCase
      uc.number should be(1)
      uc.title should be("Untitled")
    }

    // TODO New-UC has GLOBAL scope.

    it("should create the second as '2. Untitled'") {
      createNewUseCase
      val uc = createNewUseCase
      uc.number should be(2)
      uc.title should be("Untitled")
    }
  }
}
