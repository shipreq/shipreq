package com.beardedlogic.usecase.integration

import org.scalatest.FunSuite
import scala.slick.jdbc.StaticQuery
import com.beardedlogic.usecase.lib.db.DB
import com.beardedlogic.usecase.test.fixture.UserFixture
import support.SeleniumTest

class LoginTest extends FunSuite with SeleniumTest with UserFixture {

  val LoginCount = StaticQuery.query[Long, Int]("select login_count from usr where id=?")

  override def beforeAll {
    super.beforeAll
    initUserFixture()
  }

  def loginCountFor(u: TestUser) = DB.withInstance(false)(db => LoginCount.first(u.id)(db))

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
    val origLoginCount = loginCountFor(user1)
    goto.login
    .enterUsername(user1.username)
    .enterPassword(user1.password)
    .setRememberMe(false)
    .login()
    eventually {currentUrl should be("/")}
    loginCountFor(user1) should be(origLoginCount + 1)
  }
}
