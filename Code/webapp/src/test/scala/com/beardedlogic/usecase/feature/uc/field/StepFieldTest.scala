package com.beardedlogic.usecase
package feature.uc
package field

import org.scalatest.FunSpec
import lib.Types._
import test.{TestData, TestHelpers}

class StepFieldTest extends FunSpec with TestHelpers with TestData {

  describe("Field.apply()") {
    it("should lookup the field value and cast result") {
      val tf1 = freeText("1")
      val tf2 = freeText("2")
      val m: FieldValues = Map(TF2 ~> tf1, TF3 ~> tf2, NCF ~> NCF.empty)
      val r2: StepFieldValue = NCF(m)
      r2 ==== NCF.empty
    }
  }
}