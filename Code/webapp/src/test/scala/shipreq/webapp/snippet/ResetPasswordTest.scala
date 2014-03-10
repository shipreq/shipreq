package shipreq.webapp.snippet

import net.liftweb.util.Helpers._
import org.joda.time.DateTime
import org.mockito.Mockito.{when, verify}
import org.scalatest.FunSpec

import shipreq.webapp.db.{ResetPasswordInfo, UserRegistrationInfo, DaoT}
import shipreq.webapp.lib.Types._
import shipreq.webapp.test.T2._
import shipreq.webapp.test.{MockDaoProvider, TestHelpers}
import shipreq.webapp.util.NonEmptyTemplate

class ResetPasswordTest extends FunSpec with TestHelpers {

  val validEmail = "hehe@yay.com"

  val expiredTime = 2 days
  val nonExpiredTime = 1 hour

  describe("ResetPassword1.perform") {

    def findUserReturns(r: Option[(UserRegistrationInfo, ResetPasswordInfo)]): DbSetup = new DbSetup {
      override def setup(d: DaoT) = when(d.findUserRegAndResetPwInfo(any)) thenReturn r
    }
    def registeredUser = UserRegistrationInfo(5.tag, None, None, Some(DateTime.now))
    def existingToken(token: String, age: TimeSpan) = ResetPasswordInfo(Some(token), Some(age.ago))
    def noResetPwToken = ResetPasswordInfo(None, None)

    val UserNotFound             = findUserReturns(None)
    val UnactivatedUser          = findUserReturns(Some(UserRegistrationInfo(5.tag, Some("X"), Some((1 minute).ago), None), noResetPwToken))
    val UserWithoutExistingToken = findUserReturns(Some(registeredUser, noResetPwToken))
    val UserWithExpiredToken     = findUserReturns(Some(registeredUser, existingToken("EXPIRED", expiredTime)))
    val UserWithValidToken       = findUserReturns(Some(registeredUser, existingToken("VALID", nonExpiredTime)))

    val JsEmailSent     = NoErrorNotice & JsContains("resetpwTokenSent")
    val JsEmailRejected = HasErrorNotice("mail address") & JsDoesntContain("resetpwTokenSent")

    val ChangeReqEmailSent            = EmailSentContaining("new password here:", "http", "/resetpw/")
    val ConfirmRegistrationEmailSent  = EmailSentContaining("To continue your registration", "http")

    def NoDbChange     = new DbCheck(false, false)
    def IssuesNewToken = new DbCheck(true, false)
    def ReusesToken    = new DbCheck(false, true)
    class DbCheck(newToken: Boolean, reuse: Boolean) extends DbExp {
      override def test(d: DaoT) = {
        verifyO(d, newToken).performInstallNewResetPasswordToken(any, any)
        verifyO(d, reuse).performReuseResetPasswordToken(any)
      }
    }

    def test(emailInput: String, dbSetup: DbSetup)(dbExp: DbExp, emailExp: EmailExp, jsExp: JsExp): Unit =
      inMockSession {
        val r = withTestMailer {
          MockDaoProvider(dao => {
            when(dao.performInstallNewResetPasswordToken(any, any)) thenReturn "TOKEN"
            dbSetup setup dao
          }).install {
            val r = ResetPassword1.perform(emailInput)
            dbExp.test()
            r
          }
        }
        jsExp test r.result
        emailExp test r
      }

    // matches user: N, valid email: Y - pretend sent
    it("should do nothing and pretend email sent when no user found and email is valid") {
      test(validEmail, UserNotFound)(NoDbChange, NoEmailSent, JsEmailSent)
    }

    // matches user: N, valid email: N - error
    it("should reject email address when no user found and email is invalid") {
      test("invalidEmail", UserNotFound)(NoDbChange, NoEmailSent, JsEmailRejected)
    }

    // matches user: Y, registered: N - send reg token
    it("should send registration email when user found and account not activated") {
      test(validEmail, UnactivatedUser)(NoDbChange, ConfirmRegistrationEmailSent, JsEmailSent)
    }

    // matches user: Y, registered: Y, has recent reset pw token: Y - reuse token, send email, inc req count
    it("should send email with current token when user found and valid reset-pw token exists") {
      test(validEmail, UserWithValidToken)(ReusesToken, ChangeReqEmailSent, JsEmailSent)
    }

    // matches user: Y, registered: Y, has recent reset pw token: N - new token, send email, inc req count
    it("should send email with new token when user found and reset-pw token has expired") {
      test(validEmail, UserWithExpiredToken)(IssuesNewToken, ChangeReqEmailSent, JsEmailSent)
    }

    // matches user: Y, registered: Y, has recent reset pw token: N - new token, send email, inc req count
    it("should send email with new token when user found and doesnt have a reset-pw token yet") {
      test(validEmail, UserWithoutExistingToken)(IssuesNewToken, ChangeReqEmailSent, JsEmailSent)
    }
  }

  describe("ResetPassword2.render") {
    lazy val template = NonEmptyTemplate.load("resetpw2").get

    def findToken(r: Option[DateTime]): DbSetup =
      new DbSetup {override def setup(d: DaoT) = when(d.findResetPasswordTokenIssuedDate(any)) thenReturn r}

    val TokenNotFound = findToken(None)
    val TokenExpired = findToken(Some(expiredTime.ago))
    val TokenValid = findToken(Some(nonExpiredTime.ago))

    def test(dbSetup: DbSetup)(ne: SNoticeExp, re: RenderExp): Unit =
      inMockSession {
        MockDaoProvider(dbSetup setup _).install {
          val s = new ResetPassword2("ah")
          val r = tryRender(s.render(template))
          ne.test()
          re.test(r)
        }
      }

    it("should redirect when token expired") {
      test(TokenExpired)(HasErrorNoticeContaining("expired"), Redirects)
    }
    it("should redirect when token not found") {
      test(TokenNotFound)(HasErrorNoticeContaining("invalid"), Redirects)
    }
    it("should provide form when token valid") {
      test(TokenValid)(NoNotices, HtmlContains("<form "))
    }
  }

  describe("ResetPassword2.onSubmit") {

    val UpdatesPassword = new DbExp {override def test(d: DaoT) = verify(d).performPasswordReset(any, any)}

    def test(p: String)(dbExp: DbExp, jsExp: JsExp): Unit =
      inMockSession {
        MockDaoProvider().install {
          val s = new ResetPassword2("ah")
          s.password1Input = p
          s.password2Input = p
          val js = s.onSubmit()
          dbExp.test()
          jsExp test js
        }
      }

    it("should reject invalid passwords") {
      test("x")(NoDbInteraction, HasErrorNotice("assword"))
    }
    it("should update the password when valid") {
      test("asdjhf2314sdfajk")(UpdatesPassword, JsContains("toggle"))
    }
  }
}
