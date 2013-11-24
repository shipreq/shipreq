package com.beardedlogic.usecase
package feature.uc.field

import org.scalatest.FunSpec
import feature.uc.text.FreeText
import lib.Types._
import test.TestHelpers

class TextFieldTest extends FunSpec with TestHelpers {

  describe("Field.apply()") {
    it("should lookup the field value and cast result") {
      val tf1 = freeText("1")
      val tf2 = freeText("2")
      val m: FieldValues = Map(TF2 ~> tf1, TF3 ~> tf2, NCF ~> NCF.empty)
      var r: FreeText = TF2(m)
      r shouldBe tf1
      r = TF3(m)
      r shouldBe tf2
    }
  }
}
