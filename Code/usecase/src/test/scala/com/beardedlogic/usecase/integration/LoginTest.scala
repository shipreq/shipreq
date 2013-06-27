package com.beardedlogic.usecase.integration

import org.scalatest.FunSuite
import support.SeleniumTest
import com.beardedlogic.usecase.test.fixture.UserFixture

class LoginTest extends FunSuite with SeleniumTest with UserFixture {

  override def beforeAll {
    super.beforeAll
    initUserFixture()
  }

  test("Failed login") {
    goto.login
    .enterUsername(user1.username)
    .enterPassword("blah12345689754asdasf")
    .setRememberMe(false)
    .login()
    eventually {selenium.getPageSource should include("Invalid login")}
    currentUrl should be("/login")
  }

  test("Successful login") {
    goto.login
    .enterUsername(user1.username)
    .enterPassword(user1.password)
    .setRememberMe(false)
    .login()
    eventually {currentUrl should be("/")}
  }
}
