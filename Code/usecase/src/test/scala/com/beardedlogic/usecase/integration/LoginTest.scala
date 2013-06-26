package com.beardedlogic.usecase.integration

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import com.beardedlogic.usecase.test.TestHelpers
import com.beardedlogic.usecase.test.fixture.UserFixture

class LoginTest
  extends FunSuite
          with ShouldMatchers
          with SeleniumDSL
          with TestHelpers
          with UserFixture {

  override def beforeAll {
    super.beforeAll
    initUserFixture()
  }

  test("Failed login") {
    gotoLogin
    .enterUsername(user1.username)
    .enterPassword("blah12345689754asdasf")
    .setRememberMe(false)
    .login()
    eventually {s.getPageSource should include("Invalid login")}
    currentUrl should be("/login")
  }

  test("Successful login") {
    gotoLogin
    .enterUsername(user1.username)
    .enterPassword(user1.password)
    .setRememberMe(false)
    .login()
    eventually {currentUrl should be("/")}
  }
}
