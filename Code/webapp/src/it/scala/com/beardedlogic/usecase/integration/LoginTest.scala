package shipreq.webapp.integration

import org.scalatest.FunSuite
import scala.slick.jdbc.StaticQuery
import shipreq.webapp.test.TestDB
import shipreq.webapp.test.fixture.UserFixture
import support.SeleniumTest

class LoginTest extends FunSuite with SeleniumTest with UserFixture {

  val LoginCount = StaticQuery.query[Long, Int]("select login_count from usr where id=?")

  override def beforeAll {
    super.beforeAll
    initUserFixtureWithoutTransaction
  }

  override def afterAll {
    deleteUserFixtureWithoutTransaction
    super.afterAll
  }

  def loginCountFor(u: TestUser) = TestDB.withInstance(false)(db => LoginCount.first(u.id)(db))

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
