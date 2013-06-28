package com.beardedlogic.usecase
package snippet

import net.liftweb.common.Empty
import net.liftweb.http.{LiftSession, S}
import net.liftweb.util.StringHelpers
import org.scalatest.FunSpec
import test.TestDatabaseSupport
import test.fixture.UserFixture

/*
Screen 2:
Token
- not provided => show screen #1
- valid => show form
- invalid => Alert invalid, show screen #1
- expired => Alert expired, show screen #1

Form
: Username
: Password 1 & 2
*/

class RegisterSnippetTest extends FunSpec with TestDatabaseSupport with UserFixture {

  def inMockSession[U](block: => U): U = {
    val session: LiftSession = new LiftSession("", StringHelpers.randomString(20), Empty)
    S.initIfUninitted(session) {block}
  }

  override def beforeEachWithDao() {
    initUserFixture(session)
  }

  class Req1Tester extends SnippetTesterWithDao(new Register1) {

    def submit(email: String, usrTableDiff: Int) = {
      snippet.emailInput = email
      assertTableDiffs('usr -> usrTableDiff) {snippet.onSubmit(js.reactor)}
      this
    }

    def assertError(errorMsg: Option[String]) = {
      if (errorMsg.isDefined)
        jsReaction should include(errorMsg.get)
      else
        jsReaction.toLowerCase should not include ("alert")
      this
    }

    def assertEmail(emailFrags: Option[List[String]]) = {
      testListOfZeroOrOne(emailFrags, mailer.sent)(mail =>
        for (f <- emailFrags.get) mail.getContent.toString should include(f)
      )
      this
    }
  }

  def testSuccess(email: String, usrTableDiff: Int, tokenChange: Boolean) {
    val tokenBefore = lookupConfirmationToken(email)
    val tester = new Req1Tester()
    tester.submit(email, usrTableDiff).assertError(None)
    val token = lookupConfirmationToken(email)
    token should not be ('empty)
    if (tokenChange)
      token should not be (tokenBefore)
    else
      token should be(tokenBefore)
    tester.assertEmail(Some(List(token.get)))
  }

  describe("Request1.onSubmit") {
    it("when email is invalid -- should reject request") {
      new Req1Tester().submit("not_an_email", 0)
      .assertError(Some("valid email"))
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
      new Req1Tester().submit(user1.email, 0)
      .assertError(None)
      .assertEmail(Some(List("/login")))
    }
  }
}
