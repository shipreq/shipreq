package com.beardedlogic.usecase.integration

import org.openqa.selenium.Keys
import org.scalatest.{BeforeAndAfter, FunSuite}
import com.beardedlogic.usecase.lib.{ExternalId, Defaults}
import com.beardedlogic.usecase.test.{TestDatabaseSupport, TestHelpers}
import support.SeleniumTest

class UseCaseIndexTest extends FunSuite with SeleniumTest with BeforeAndAfter with TestDatabaseSupport {

  override val wrapTestsInTransaction = false

  lazy val dsl = goto.useCaseIndex

  def assertDatabase(expected: (Int, String)*) {
    db.findAllUseCaseSummaries.map(s => (s.number.toInt, s.title)).toList should be(expected.toList)
  }

  def assertLinkUrl() {
    val valueId = db.findAllUseCaseSummaries.head.valueId
    val dataId = db.findUseCase(valueId).get.identId
    dsl.row(0).linkUrl should be(baseUrl + "/usecase/" + ExternalId(dataId))
  }

  test("empty initially") {
    dsl.assertItemCount(0)
  }

  test("adding UC") {
    dsl.clickNewUc().assertItemCount(1, 1).row(0).assertEditText(Defaults.Title)
    assertDatabase((1, Defaults.Title))
    10.times(keyboard.sendKeys(Keys.BACK_SPACE))
    keyboard.sendKeys("OMG\n")
    dsl.assertItemCount(1, 0).row(0).assertLinkText("UC-1: OMG")
    assertDatabase((1, "OMG"))
    assertLinkUrl()
  }

  test("reloading page") {
    dsl.reload.assertItemCount(1, 0).row(0).assertLinkText("UC-1: OMG")
    assertLinkUrl()
  }
}
