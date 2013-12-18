package com.beardedlogic.shipreq
package snippet

import net.liftweb.http.{ResponseShortcutException, S}
import net.liftweb.util.Helpers.intToTimeSpanBuilder
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken
import org.postgresql.util.PSQLException
import org.scalatest.FunSpec

import security.Oshiro
import test.{TestMailer, TestDatabaseSupport}
import test.fixture.UserFixture
import util.NonEmptyTemplate
import app.AppConfig
import Register._

class RegisterSnippetTest extends FunSpec with TestDatabaseSupport with UserFixture {

  override def beforeEachWithDao() {
    initUserFixture(session)
  }

  lazy val reg1html = NonEmptyTemplate.load("register").get

  def assertSingleError(substring: String) {
    S.errors.size should be(1)
    S.errors(0)._1.toString.toLowerCase should include(substring.toLowerCase)
  }

  describe("isTokenExpired") {
    it("should consider 1-day-old valid") {
      isTokenExpired(1.day.ago) should be(false)
    }
    it("should consider 1-week-old expired") {
      isTokenExpired(1.week.ago) should be(true)
    }
  }

  describe("Register1.render") {
    def test(config: Boolean, allowed: Boolean): Unit = inMockSession {
      val orig = AppConfig.AllowRegister
      try {
        AppConfig.AllowRegister = () => config
        val x = Register1.render(reg1html)
        val h = x.toString
        h.contains("register1Form") shouldBe allowed
        h.contains("registrationDisabled") shouldBe (!allowed)
      } finally {
        AppConfig.AllowRegister = orig
      }
    }

    it("should allow registration to anonymous user when config on") {
      test(true, true)
    }
    it("should deny registration to anonymous user when config off") {
      test(false, false)
    }
    it("should deny registration to non-admin user when config off") {
      login(user2)
      test(false, false)
    }
    it("should allow registration to admin even when config off") {
      login(user1)
      test(false, true)
    }
  }

  describe("Register1.onSubmit") {

    def test(email: String, usrTableDiff: Int) =
      withTestMailer {
        assertTableDiffs(Tables.Usr -> usrTableDiff) {
          Register1.perform(email)
    }}

    def testSuccess(email: String, usrTableDiff: Int, tokenChange: Boolean) {
      val tokenBefore = lookupConfirmationToken(email)
      val r = test(email, usrTableDiff)
      r.result.assertJsAlert(None)
      val token = lookupConfirmationToken(email)
      token should not be ('empty)
      if (tokenChange)
        token should not be (tokenBefore)
      else
        token should be(tokenBefore)
      r.assertEmail(Some(List(token.get)))
    }

    it("when email is invalid -- should reject request") {
      val r = test("not_an_email", 0)
      r.result.assertJsAlert(Some("Email"))
      r.assertEmail(None)
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
      val r = test(user1.email, 0)
      r.result.assertJsAlert(None)
      r.assertEmail(Some(List("/login")))
    }
  }

  // ===================================================================================================================

  class Reg2Tester(token: String) {
    val snippet = new Register2(token)
    def onSubmit_() = TestMailer.install(snippet.onSubmit()).result
  }

  describe("Register2.validateToken") {
    it("should redirect to Register1 with error when token is invalid") {
      inMockSession {
        intercept[ResponseShortcutException] {new Reg2Tester("blah").snippet.validateToken_!}
        assertSingleError("invalid")
      }
    }

    it("should redirect to Register1 with error when token has expired") {
      inMockSession {
        intercept[ResponseShortcutException] {new Reg2Tester(userWithExpiredToken.token).snippet.validateToken_!}
        assertSingleError("expired")
      }
    }

    it("should render new-user form when token is valid") {
      inMockSession {
        new Reg2Tester(userWithCurrentToken.token).snippet.validateToken_!
        S.errors shouldBe empty
      }
    }
  }

  describe("Register2 POST") {
    def tester = {
      val t = new Reg2Tester(userWithCurrentToken.token)
      t.snippet.usernameInput = "crazy50"
      t.snippet.password1Input = "abcd5678"
      t.snippet.password2Input = t.snippet.password1Input
      t
    }

    def assertUnconfirmed() {
      val reg = dao.findUserRegistrationInfo(userWithCurrentToken.email).get
      reg.confirmationSentAt should be(Some(userWithCurrentToken.tokenCreatedAt))
      reg.confirmedAt should be(None)
      reg.confirmationToken should be(Some(userWithCurrentToken.token))
    }

    def testFailure(mutate: Register2 => Any) {
      val t = tester
      mutate(t.snippet)
      val js = t.onSubmit_
      assertUnconfirmed()
      js.assertJsAlert(Some(""))
    }

    it("should reject an invalid username") {
      testFailure(_.usernameInput = "9000")
    }

    it("should reject an invalid password") {
      testFailure(s => {s.password1Input = "abcd"; s.password2Input = "abcd"})
    }

    it("should reject when passwords dont match") {
      testFailure(_.password1Input = "987654321zcbsdfg")
    }

    it("should reject a taken username") {
      val t = tester
      t.snippet.usernameInput = user2.username
      t.onSubmit_
      try {assertUnconfirmed()}
      catch {case e: PSQLException if e.getMessage.contains("transaction is aborted") =>}
    }

    describe("when form details valid") {
      it("should create user") {
        tester.onSubmit_
        val reg = dao.findUserRegistrationInfo(userWithCurrentToken.email).get
        reg.confirmationSentAt should be(Some(userWithCurrentToken.tokenCreatedAt))
        reg.confirmedAt should not be (None)
        reg.confirmedAt.get.after(1.minute.ago) should be(true)
        reg.confirmationToken should be(None)

        val (user, pwd) = dao.findUserDescAndCredentials(userWithCurrentToken.email).get
        user.username should be("crazy50")
        pwd.hashedPassword should not be ("abcd5678")
      }

      it("should login") {
        Oshiro.loggedInUser should be(None)
        tester.onSubmit_
        Oshiro.loggedInUser should not be (None)
        val user = Oshiro.loggedInUser.get
        user.username should be("crazy50")
        user.email should be(userWithCurrentToken.email)
      }

      it("should hash password so that login auth works with same plaintext password") {
        val subj = SecurityUtils.getSubject
        tester.onSubmit_
        subj.logout()
        subj.login(new UsernamePasswordToken("crazy50", "abcd5678"))
      }

      it("should hide the form and show the success") {
        val t = tester
        val js = t.onSubmit_
        js.assertJsAlert(None)
        js.toJsCmd should include("toggle")
      }
    }
  }
}
