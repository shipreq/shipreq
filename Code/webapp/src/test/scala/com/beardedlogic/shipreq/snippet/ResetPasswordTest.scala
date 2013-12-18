package com.beardedlogic.shipreq.snippet

import net.liftweb.util.Helpers._
import org.joda.time.DateTime
import org.mockito.Mockito.when
import org.scalatest.FunSpec

import com.beardedlogic.shipreq.db.{ResetPasswordInfo, UserRegistrationInfo, DaoT}
import com.beardedlogic.shipreq.lib.Types._
import com.beardedlogic.shipreq.test.T2._
import com.beardedlogic.shipreq.test.{MockDaoProvider, TestHelpers}

class ResetPasswordTest extends FunSpec with TestHelpers {

  describe("Request submission") {

    def findUserReturns(r: Option[(UserRegistrationInfo, ResetPasswordInfo)]): DbSetup = new DbSetup {
      override def setup(d: DaoT) = when(d.findUserRegAndResetPwInfo(any)) thenReturn r
    }
    def registeredUser = UserRegistrationInfo(5.tag, None, None, Some(DateTime.now))
    def existingToken(token: String, age: TimeSpan) = ResetPasswordInfo(Some(token), Some(age.ago))
    def noResetPwToken = ResetPasswordInfo(None, None)

    val UserNotFound             = findUserReturns(None)
    val UnactivatedUser          = findUserReturns(Some(UserRegistrationInfo(5.tag, Some("X"), Some((1 minute).ago), None), noResetPwToken))
    val UserWithoutExistingToken = findUserReturns(Some(registeredUser, noResetPwToken))
    val UserWithExpiredToken     = findUserReturns(Some(registeredUser, existingToken("EXPIRED", 1 month)))
    val UserWithValidToken       = findUserReturns(Some(registeredUser, existingToken("VALID", 1 minute)))

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
      test("hehe@yay.com", UserNotFound)(NoDbChange, NoEmailSent, JsEmailSent)
    }

    // matches user: N, valid email: N - error
    it("should reject email address when no user found and email is invalid") {
      test("what", UserNotFound)(NoDbChange, NoEmailSent, JsEmailRejected)
    }

    // matches user: Y, registered: N - send reg token
    it("should send registration email when user found and account not activated") {
      test("hehe@yay.com", UnactivatedUser)(NoDbChange, ConfirmRegistrationEmailSent, JsEmailSent)
    }

    // matches user: Y, registered: Y, has recent reset pw token: Y - reuse token, send email, inc req count
    it("should send email with current token when user found and valid reset-pw token exists") {
      test("hehe@yay.com", UserWithValidToken)(ReusesToken, ChangeReqEmailSent, JsEmailSent)
    }

    // matches user: Y, registered: Y, has recent reset pw token: N - new token, send email, inc req count
    it("should send email with new token when user found and reset-pw token has expired") {
      test("hehe@yay.com", UserWithExpiredToken)(IssuesNewToken, ChangeReqEmailSent, JsEmailSent)
    }

    // matches user: Y, registered: Y, has recent reset pw token: N - new token, send email, inc req count
    it("should send email with new token when user found and doesnt have a reset-pw token yet") {
      test("hehe@yay.com", UserWithoutExistingToken)(IssuesNewToken, ChangeReqEmailSent, JsEmailSent)
    }
  }
}
