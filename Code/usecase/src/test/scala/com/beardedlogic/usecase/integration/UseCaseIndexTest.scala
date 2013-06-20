package com.beardedlogic.usecase.integration

import org.scalatest.{GivenWhenThen, BeforeAndAfter, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import com.beardedlogic.usecase.test.{TestDatabaseSupport, TestHelpers}
import com.beardedlogic.usecase.lib.{ExternalId, Defaults}
import org.openqa.selenium.Keys
import TestHelpers._

class UseCaseIndexTest extends FunSuite
                               with ShouldMatchers
                               with SeleniumDSL
                               with TestHelpers
                               with BeforeAndAfter
                               with GivenWhenThen
                               with TestDatabaseSupport {

  override val wrapTestsInTransaction = false

  lazy val dsl = listDsl

  def assertDatabase(expected: (Int, String)*) {
    db.findAllUseCaseSummaries.map(s => (s.number.toInt, s.title)).toList should be(expected.toList)
  }

  def assertLinkUrl() {
    val valueId = db.findAllUseCaseSummaries.head.valueId
    val dataId = db.findUseCase(valueId).get.dataId
    dsl.row(0).linkUrl should be(baseUrl + "/usecase/" + ExternalId(dataId))
  }

  test("empty initially") {
    dsl.assertItemCount(0)
  }

  test("adding UC") {
    dsl.clickNewUc().assertItemCount(1, 1).row(0).assertEditText(Defaults.Title)
    assertDatabase((1, Defaults.Title))
    10.times(s.getKeyboard.sendKeys(Keys.BACK_SPACE))
    s.getKeyboard.sendKeys("OMG\n")
    dsl.assertItemCount(1, 0).row(0).assertLinkText("UC-1: OMG")
    assertDatabase((1, "OMG"))
    assertLinkUrl()
  }

  test("reloading page") {
    dsl.reload.assertItemCount(1, 0).row(0).assertLinkText("UC-1: OMG")
    assertLinkUrl()
  }
}
