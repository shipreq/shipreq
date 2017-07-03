package shipreq.webapp.server.logic

import java.time.Duration
import scalaz.{-\/, Name, \/, \/-}
import utest._
import shipreq.base.util._
import shipreq.taskman.api.Msg
import shipreq.webapp.base.data.SecurityToken
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.user._
import shipreq.webapp.client.public.PublicSpaProtocols._

object PublicSpaLogicTest extends TestSuite {

  class Tester(allowRegister: Permission = Allow) extends MockInterpreters(_.copy(allowRegister = allowRegister)) {
    val logic = PublicSpaLogic[Name, Name]
    val initData = logic.initData.value

    def forwardTimeToEndOfConfirmationWindow(v: Validity): Unit =
      svr.forwardTimeToEndOfWindow(config.confirmationTokenLifespan, v)

    def forwardTimeToEndOfPasswordResetWindow(v: Validity): Unit =
      svr.forwardTimeToEndOfWindow(config.passwordResetTokenLifespan, v)

    def runRegister1 (i: Register.Fn1 .Input): Register.Fn1 .Response = assertProtected(svr.run(initData.register1)(i))
    def runRegister2A(i: Register.Fn2A.Input): Register.Fn2A.Response = assertProtected(svr.run(initData.register2A)(i))
    def runRegister2B(i: Register.Fn2B.Input): Register.Fn2B.Response = assertProtected(svr.run(initData.register2B)(i))

    def runResetPassword1 (i: ResetPassword.Fn1 .Input): ResetPassword.Fn1 .Response = assertProtected(svr.run(initData.resetPassword1)(i))
    def runResetPassword2A(i: ResetPassword.Fn2A.Input): ResetPassword.Fn2A.Response = assertProtected(svr.run(initData.resetPassword2A)(i))
    def runResetPassword2B(i: ResetPassword.Fn2B.Input): ResetPassword.Fn2B.Response = assertProtected(svr.run(initData.resetPassword2B)(i))

    val user2 = MockDb.UserEntry(
      UserId(7),
      Username("blurp"),
      EmailAddr("blurp@bar.com"),
      security.hashPassword(PlainTextPassword("blurp12345")).value,
      svr.clock minus Duration.ofDays(5))

    db.users ::= user2
  }

  val ea = EmailAddr("blah@test.com")

  def assertRegistrationEmailSent(emailAddr: EmailAddr = ea)(implicit t: Tester): Unit = {
    import t._
    val m = taskman.assertLastSubmitted { case m: Msg.RegistrationRequested => m }
    assertEq(m.email.value, ea.value)
    assertContains(m.verifyEmailUrl, db.prevToken().value)
  }

  override def tests = TestSuite {

    'register1 {
      implicit val t = new Tester(); import t._

      def runSuccessfully(tokensIssued: Int, msgsSubmitted: Int, emailAddr: EmailAddr = ea): Unit =
        db.assertIssuesTokens(tokensIssued)(
          taskman.assertSubmits(msgsSubmitted)(
            runRegister1(emailAddr).needRight))

      "email is valid and new -- should create a user, token and send email" - {
        runSuccessfully(1, 1)
        assertRegistrationEmailSent()
      }

      "pending, valid token exists -- should resend email" - {
        runSuccessfully(1, 1)
        forwardTimeToEndOfConfirmationWindow(Valid)
        runSuccessfully(0, 1)
        assertRegistrationEmailSent()
      }

      "pending, expired token exists -- should create a new token and email" - {
        runSuccessfully(1, 1)
        forwardTimeToEndOfConfirmationWindow(Invalid)
        runSuccessfully(1, 1)
        assertRegistrationEmailSent()
      }

      "email belongs to registered account -- should email with link to reset password" - {
        runSuccessfully(0, 1, user2.emailAddr)
        val m = taskman.assertLastSubmitted { case m: Msg.ReRegistrationAttempted => m }
        assertEq(m.email.value, user2.emailAddr.value)
      }

      "email is invalid -- should reject request" - {
        runRegister1(EmailAddr("not_an_email")).needLeft
        taskman.assertSubmitted(0)
      }

      'registrationsOff {
        val t = new Tester(Deny); import t._
        runRegister1(ea).needLeft
      }
    }

    'register2B {
      import Register._
      val t = new Tester(); import t._

      // Mock user (pending)
      runRegister1(ea).needRight
      val token = db.prevToken()
      val req = Request(token, PersonName("Big Bob"), Username("bob"), PlainTextPassword("big_BOB_123!"), false)

      'success - {
        assertDifference("taskman", taskman.msgs.length)(1) {
          assertDifference("userPlaceholders", db.userPlaceholders.size)(-1)(
            assertDifference("users", db.users.length)(1)(
              assertEq(\/-(Response.Success), runRegister2B(req))))
          svr.runForked()
        }
        assertEq(security.loggedIn.map(_.username), Some(req.username))
        taskman.assertLastSubmitted { case r: Msg.RegistrationCompleted => () }
      }

      def assertFailure(req: Request): Fn2B.Response =
        assertDifference(db.userPlaceholders.size)(0)(
          assertDifference(db.users.length)(0)(
            assertDifference(taskman.msgs.length)(0)(
              runRegister2B(req))))

      "reject an invalid name" -
        assertFailure(req.copy(personName = PersonName(""))).needLeft

      "reject an invalid username" -
        assertFailure(req.copy(username = Username("9000"))).needLeft

      "reject an invalid password" -
        assertFailure(req.copy(password = PlainTextPassword("abc"))).needLeft

      "reject a taken username" -
        assertEq(assertFailure(req.copy(username = user2.username)), \/-(Response.UsernameTaken))

      'registrationsOff {
        val t = new Tester(Deny); import t._
        db.userPlaceholders = Map(ea -> DB.UserRegistration.Pending(UserId(2), token, svr.clock))
        runRegister2B(req).needLeft
      }
    }

    'resetPassword1 {
      def runSuccessfully(id: Username \/ EmailAddr, tokensIssued: Int, msgsSubmitted: Int)(implicit t: Tester): Unit = {
        import t._
        db.assertIssuesTokens(tokensIssued)(
          taskman.assertSubmits(msgsSubmitted)(
            runResetPassword1(id).needRight))
      }

      def assertResetPasswordEmailSent()(implicit t: Tester): Unit = {
        import t._
        val m = taskman.assertLastSubmitted { case m: Msg.PasswordResetRequested => m }
        assertEq(m.email.value, user2.emailAddr.value)
        assertContains(m.resetPasswordUrl, db.prevToken().value)
        assertEq(db.getUser(user2.pubids.head).get.resetPassword.map(_._1), Some(db.prevToken()))
      }

      "send email with new token when user found and doesn't have a reset-pw token yet" -
        List(0, 1).foreach { i =>
          implicit val t = new Tester(); import t._
          val u = user2.pubids(i)
          runSuccessfully(u, 1, 1)
          assertResetPasswordEmailSent()
        }

      "send email with current token when user found and valid reset-pw token exists" -
        List(0, 1).foreach { i =>
          implicit val t = new Tester(); import t._
          val u = user2.pubids(i)
          runSuccessfully(u, 1, 1)
          forwardTimeToEndOfPasswordResetWindow(Valid)
          runSuccessfully(u, 0, 1)
          assertResetPasswordEmailSent()
        }

      "send email with new token when user found and reset-pw token has expired" -
        List(0, 1).foreach { i =>
          implicit val t = new Tester(); import t._
          val u = user2.pubids(i)
          runSuccessfully(u, 1, 1)
          forwardTimeToEndOfPasswordResetWindow(Invalid)
          runSuccessfully(u, 1, 1)
          assertResetPasswordEmailSent()
        }


      "do nothing and seem ok when no user found" - {
        val t = new Tester(); import t._
        db.assertNoDbChange(
          assertDifference(taskman.msgs.length)(0) {
            runResetPassword1(-\/(Username("xxxxxxxxxxxxxx"))).needRight
            runResetPassword1(\/-(EmailAddr("xxxxxxxxx@xxxxx.com"))).needRight
          }
        )
      }

      "send registration email when user found and account not activated" - {
        implicit val t = new Tester(); import t._
        assertDifference(db.userPlaceholders.size)(1)(runRegister1(ea).needRight)
        runSuccessfully(\/-(ea), 0, 1)
        assertRegistrationEmailSent()
      }
    }

    'resetPassword2 {
      import ResetPassword._
      implicit val t = new Tester(); import t._
      val i = \/-(user2.emailAddr)
      runResetPassword1(i).needRight
      val token = db.prevToken()
      val p2 = PlainTextPassword("asdjhf2314sdfajk")

      "update the password when valid" - {
        assertEq(security.attemptLogin(i, p2).value, Deny)
        assertEq(runResetPassword2B(Request(token, p2)), \/-(Response.Success))
        assertEq(security.attemptLogin(i, p2).value, Allow)
      }

      "reject invalid passwords" -
        runResetPassword2B(Request(token, PlainTextPassword("x"))).needLeft

      "reject invalid tokens" -
        assertEq(runResetPassword2B(Request(SecurityToken("xxxx"), p2)), \/-(Response.TokenInvalid))

      "reject expired tokens" - {
        forwardTimeToEndOfPasswordResetWindow(Invalid)
        assertEq(runResetPassword2B(Request(token, p2)), \/-(Response.TokenExpired))
      }
    }

  }
}
