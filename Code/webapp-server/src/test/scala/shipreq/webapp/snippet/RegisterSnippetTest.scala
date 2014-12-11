package shipreq.webapp
package snippet

import net.liftweb.http.{ResponseShortcutException, S}
import net.liftweb.util.Helpers.intToTimeSpanBuilder
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken
import org.postgresql.util.PSQLException
import org.scalatest.FunSpec

import shipreq.base.util.ScalaExt._
import security.Oshiro
import test.TestDatabaseSupport
import test.fixture.UserFixture
import util.NonEmptyTemplate
import app.AppConfig
import Register._
import shipreq.webapp.test.T2._
import shipreq.taskman.api.Msg.RegistrationRequested
import shipreq.webapp.feature.validation.Validators

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
      withTestTaskman {
        assertTableDiffs(Tables.Usr -> usrTableDiff) {
          Register1.perform(Validators.email.correctAndValidateU(email))
    }}

    def testSuccess(email: String, usrTableDiff: Int, tokenChange: Boolean) {
      val tokenBefore = lookupConfirmationToken(email)
      val (r, tt) = test(email, usrTableDiff)
      r.assertJsAlert(None)
      val token = lookupConfirmationToken(email)
      token should not be ('empty)
      if (tokenChange)
        token should not be (tokenBefore)
      else
        token should be(tokenBefore)
      SubmittedOneTask{ case RegistrationRequested(_,url) => () => url should include(token.get) } test tt
    }

    it("when email is invalid -- should reject request") {
      val (r, tt) = test("not_an_email", 0)
      r.assertJsAlert(Some("Email"))
      NoTasksSubmitted.test(tt)
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
      val (r, tt) = test(user1.email, 0)
      r.assertJsAlert(None)
      SubmittedOneTask(ReRegistrationAttemptedT) test tt
    }
  }

  // ===================================================================================================================

  class Reg2Tester(token: String) {
    val snippet = new Register2(token)

    def name_=     (v: String) : Unit = snippet.vars = snippet.vars put1 v
    def username_= (v: String) : Unit = snippet.vars = snippet.vars put2 v
    def password1_=(v: String) : Unit = snippet.vars = snippet.vars map3 (_ put1 v)
    def password2_=(v: String) : Unit = snippet.vars = snippet.vars map3 (_ put2 v)
    def tos_=      (v: Boolean): Unit = snippet.vars = snippet.vars put5 v

    def onSubmit() = withTestTaskman(snippet.onSubmit())

    def onSubmitF() = {
      val (js, tt) = onSubmit()
      NoTasksSubmitted test tt
      js
    }
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
      t name_=      "John Stuff"
      t username_=  "crazy50"
      t password1_= "abcd5678"
      t password2_= "abcd5678"
      t tos_=       true
      t
    }

    def assertUnconfirmed() {
      val reg = dao.findUserRegistrationInfo(userWithCurrentToken.email).get
      reg.confirmationSentAt should be(Some(userWithCurrentToken.tokenCreatedAt))
      reg.confirmedAt should be(None)
      reg.confirmationToken should be(Some(userWithCurrentToken.token))
    }

    def testFailure(mutate: Reg2Tester => Any) {
      val t = tester
      mutate(t)
      val js = t.onSubmitF
      assertUnconfirmed()
      js.assertJsAlert(Some(""))
    }

    it("should reject an invalid name") {
      testFailure(_ name_= "9000")
    }

    it("should reject an invalid username") {
      testFailure(_ username_= "9000")
    }

    it("should reject an invalid password") {
      testFailure(_ password2_= "abcd")
    }

    it("should reject when passwords dont match") {
      testFailure(_ password1_= "987654321zcbsdfg")
    }

    it("should reject a taken username") {
      val t = tester
      t username_= user2.username
      t.onSubmitF
      try {assertUnconfirmed()}
      catch {case e: PSQLException if e.getMessage.contains("transaction is aborted") =>}
    }

    it("should reject without ToS agreement") {
      testFailure(_ tos_= false)
    }

    describe("when form details valid") {
      it("should create user") {
        tester.onSubmit()
        val reg = dao.findUserRegistrationInfo(userWithCurrentToken.email).get
        reg.confirmationSentAt should be(Some(userWithCurrentToken.tokenCreatedAt))
        reg.confirmedAt should not be (None)
        reg.confirmedAt.get.after(1.minute.ago) should be(true)
        reg.confirmationToken should be(None)

        val (user, pwd) = dao.findUserDescAndCredentials(userWithCurrentToken.email).get
        user.username.value shouldEqual "crazy50"
        pwd.hashedPassword.value should not be("abcd5678")
      }

      it("should login") {
        Oshiro.loggedInUser should be(None)
        tester.onSubmit()
        Oshiro.loggedInUser should not be (None)
        val user = Oshiro.loggedInUser.get
        user.username.value shouldEqual "crazy50"
        user.email should be(userWithCurrentToken.email)
      }

      it("should hash password so that login auth works with same plaintext password") {
        val subj = SecurityUtils.getSubject
        tester.onSubmit()
        subj.logout()
        subj.login(new UsernamePasswordToken("crazy50", "abcd5678"))
      }

      it("should hide the form and show the success") {
        val (js, _) = tester.onSubmit()
        js.assertJsAlert(None)
        js.toJsCmd should include("toggle")
      }

      it("should submit a msg to taskman") {
        val (_, tt) = tester.onSubmit()
        SubmittedOneTask(RegistrationCompletedT) test tt
      }
    }
  }
}
