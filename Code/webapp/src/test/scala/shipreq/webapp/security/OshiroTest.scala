package shipreq.webapp
package security

import org.apache.shiro.authc._
import org.scalatest.{BeforeAndAfterAll, FunSpec}
import test.TestDatabaseSupport
import test.fixture.UserFixture

class OshiroTest extends FunSpec with TestDatabaseSupport with BeforeAndAfterAll with UserFixture {

  override val wrapTestsInTransaction = false

  override def beforeAll {
    super.beforeAll
    initUserFixtureWithoutTransaction
  }

  override def afterAll {
    deleteUserFixtureWithoutTransaction
    super.afterAll
  }

  describe("Authentication") {
    it("should allow users by username") {
      login(user1.username, user1.password)
    }

    it("should allow users by email address") {
      login(user1.email, user1.password)
    }

    it("should deny when username/email doesnt exist") {
      intercept[UnknownAccountException](login("blah", user1.password))
    }

    it("should deny when password is incorrect") {
      intercept[IncorrectCredentialsException](login(user1.username, user2.password))
    }

    it("should deny when user hasnt completed registration") {
      intercept[UnknownAccountException](login(userWithCurrentToken.email, ""))
    }
  }

  describe("loggedInUser") {
    it("should return None when no user logged in") {
      logout
      Oshiro.loggedInUser should be(None)
    }

    it("should return user details when logged in") {
      login(user1.username, user1.password)
      Oshiro.loggedInUser should be(Some(user1.toUserDescriptor))
    }
  }
}