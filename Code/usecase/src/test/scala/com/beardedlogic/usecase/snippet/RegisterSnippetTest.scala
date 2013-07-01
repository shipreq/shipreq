package com.beardedlogic.usecase
package snippet

import net.liftweb.common.Empty
import net.liftweb.http.{ResponseShortcutException, LiftSession, S}
import net.liftweb.util.StringHelpers
import org.scalatest.FunSpec
import test.TestDatabaseSupport
import test.fixture.UserFixture

class RegisterSnippetTest extends FunSpec with TestDatabaseSupport with UserFixture {

  def inMockSession[U](block: => U): U = {
    val session: LiftSession = new LiftSession("", StringHelpers.randomString(20), Empty)
    S.initIfUninitted(session) {block}
  }

  override def beforeEachWithDao() {
    initUserFixture(session)
  }

  def assertSingleError(substring: String) {
    S.errors.size should be(1)
    S.errors(0)._1.toString.toLowerCase should include(substring.toLowerCase)
  }

  class Reg1Tester extends SnippetTesterWithDao(new Register1) {
    def submit(email: String, usrTableDiff: Int) = {
      snippet.emailInput = email
      assertTableDiffs('usr -> usrTableDiff) {snippet.onSubmit(js.reactor)}
      this
    }
  }

  def testSuccess(email: String, usrTableDiff: Int, tokenChange: Boolean) {
    val tokenBefore = lookupConfirmationToken(email)
    val tester = new Reg1Tester()
    tester.submit(email, usrTableDiff).assertJsAlert(None)
    val token = lookupConfirmationToken(email)
    token should not be ('empty)
    if (tokenChange)
      token should not be (tokenBefore)
    else
      token should be(tokenBefore)
    tester.assertEmail(Some(List(token.get)))
  }

  describe("Register1.onSubmit") {
    it("when email is invalid -- should reject request") {
      new Reg1Tester().submit("not_an_email", 0)
      .assertJsAlert(Some("valid email"))
      .assertEmail(None)
    }

    it("when a pending, valid token exists -- should resend email") {
      testSuccess(userWithCurrentToken.email, 0, false)
    }

    it("when a pending, expired token exists -- should create a new token and email") {
      testSuccess(userWithExpiredToken.email, 0, true)
    }

    it("when a email is valid and new -- should create a user, token and send email") {
      testSuccess("blarrr@yay.com", 1, true)
    }

    it("when a email belongs to registered account -- should email with link to reset password") {
      new Reg1Tester().submit(user1.email, 0)
      .assertJsAlert(None)
      .assertEmail(Some(List("/login")))
    }
  }

  class Reg2Tester(token: String) extends SnippetTesterWithDao(new Register2(token)) {
  }

  describe("Register2.validateToken") {
    it("should redirect to Register1 with error when token is invalid") {
      inMockSession {
        intercept[ResponseShortcutException]{ new Reg2Tester("blah").snippet.validateToken }
        assertSingleError("invalid")
      }
    }

    it("should redirect to Register1 with error when token has expired") {
      inMockSession {
        intercept[ResponseShortcutException]{ new Reg2Tester(userWithExpiredToken.token).snippet.validateToken }
        assertSingleError("expired")
      }
    }

    it("should render new-user form when token is valid") {
      inMockSession {
        new Reg2Tester(userWithCurrentToken.token).snippet.validateToken
        S.errors should be ('empty)
      }
    }
  }

  describe("Register2 POST") {
//    it("should reject a taken username")
//    it("should reject an invalid username")
//    it("should reject an invalid password")
//    it("should reject when passwords dont match")
    describe("when form details valid") {
      //    it("should create user")
      //    it("should login")
      //    it("should hash password so that login auth works with same plaintext password")
    }
  }
}
