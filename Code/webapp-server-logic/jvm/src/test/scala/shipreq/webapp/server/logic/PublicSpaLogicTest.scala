package shipreq.webapp.server.logic

import scalaz.{-\/, \/, \/-}
import utest._
import shipreq.base.util._
import shipreq.taskman.api.Msg
import shipreq.webapp.base.data.SecurityToken
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.user._
import shipreq.webapp.client.public.{PublicSpaEntryPoint, PublicSpaProtocols}

object PublicSpaLogicTest extends TestSuite {

  class Tester(publicRegistration: Permission = Allow) extends MockInterpreters(_.copy(publicRegistration = publicRegistration)) {
    val initData = PublicSpaEntryPoint.InitData(Allow, None)

    def runLogin(i: PublicSpaProtocols.Login.ajax.Req) = assertProtected(publicSpa.ajaxLogin(i).value)
    def runRegister1(i: PublicSpaProtocols.Register1.ajax.Req) = assertProtected(publicSpa.ajaxRegister1(i).value)
    def runRegister2(i: PublicSpaProtocols.Register2.ajax.Req) = assertProtected(publicSpa.ajaxRegister2(i).value)
    def runResetPassword1(i: PublicSpaProtocols.ResetPassword1.ajax.Req) = assertProtected(publicSpa.ajaxResetPassword1(i).value)
    def runResetPassword2(i: PublicSpaProtocols.ResetPassword2.ajax.Req) = assertProtected(publicSpa.ajaxResetPassword2(i).value)

    db.users ::= user2
  }

  val ea = EmailAddr("blah@test.com")

  def assertRegistrationEmailSent(emailAddr: EmailAddr = ea)(implicit t: Tester): Unit = {
    import t._
    val m = taskman.assertLastSubmitted { case m: Msg.RegistrationRequested => m }
    assertEq(m.email.value, ea.value)
    assertContains(m.verifyEmailUrl, db.prevToken().value)
  }

  override def tests = Tests {

    'login {
      import PublicSpaProtocols.Login._
      implicit val t = new Tester(); import t._

      def test(usernameOrEmail: Username \/ EmailAddr, password: PlainTextPassword)
              (expectResp: Permission, expectToken: Option[Security.SessionToken]) =
        assertDifference("usrLoginLog", db.usrLoginLog.length)(if (expectResp is Allow) 1 else 0) {
          val r = runLogin(Request(usernameOrEmail, password))
          assertEq(r, (expectResp, expectToken))
          svr.runForked()
        }

      'badAccountU  - test(-\/(Username("nope")), user2password)(Deny, None)
      'badAccountE  - test(\/-(EmailAddr("w@w.com")), user2password)(Deny, None)
      'badPasswordU - test(-\/(user2.username), PlainTextPassword("qweoiru1234SDFG"))(Deny, None)
      'badPasswordE - test(\/-(user2.emailAddr), PlainTextPassword("qweoiru1234SDFG"))(Deny, None)
      'successU - test(-\/(user2.username), user2password)(Allow, Some(user2.token))
      'successE - test(\/-(user2.emailAddr), user2password)(Allow, Some(user2.token))
    }

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

    'register2 {
      import PublicSpaProtocols.Register2._
      val t = new Tester(); import t._

      // Mock user (pending)
      runRegister1(ea).needRight
      val token = db.prevToken()
      val req = Request(token, PersonName("Big Bob"), Username("bob"), PlainTextPassword("big_BOB_123!"), false)

      'success - {
        val r =
          assertDifference("usrLoginLog", db.usrLoginLog.length)(1)(
            assertDifference("taskman", taskman.msgs.length)(1) {
              val r =
                assertDifference("userPlaceholders", db.userPlaceholders.size)(-1)(
                  assertDifference("users", db.users.length)(1)(
                    runRegister2(req)))
              svr.runForked()
              r
            }
          )
        val u = db.getUser(-\/(req.username)).getOrElse(sys error "User not found")
        assertEq(r, (\/-(Result.Success), Some(u.token)))
        taskman.assertLastSubmitted { case r: Msg.RegistrationCompleted => () }
      }

      def assertFailure(req: Request) = {
        val r =
          assertDifference(db.userPlaceholders.size)(0)(
            assertDifference(db.users.length)(0)(
              assertDifference(taskman.msgs.length)(0)(
                runRegister2(req))))
        assertEq(r._2, None)
        r._1
      }

      "reject an invalid name" -
        assertFailure(req.copy(personName = PersonName(""))).needLeft

      "reject an invalid username" -
        assertFailure(req.copy(username = Username("9000"))).needLeft

      "reject an invalid password" -
        assertFailure(req.copy(password = PlainTextPassword("abc"))).needLeft

      "reject a taken username" -
        assertEq(assertFailure(req.copy(username = user2.username)), \/-(Result.UsernameTaken))

      'registrationsOff {
        val t = new Tester(Deny); import t._
        db.userPlaceholders = Map(ea -> DB.UserRegistration.Pending(UserId(2), token, svr.clock))
        val r = runRegister2(req)
        r._1.needLeft
        assertEq(r._2, None)
      }
    }

    'resetPassword1 {
      def runSuccessfully(id: Username \/ EmailAddr, tokensIssued: Int, msgsSubmitted: Int)(implicit t: Tester): Unit = {
        import t._
        db.assertIssuesTokens(tokensIssued)(
          taskman.assertSubmits(msgsSubmitted)(
            runResetPassword1(id)))
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
            runResetPassword1(-\/(Username("xxxxxxxxxxxxxx")))
            runResetPassword1(\/-(EmailAddr("xxxxxxxxx@xxxxx.com")))
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
      import PublicSpaProtocols.ResetPassword2._
      implicit val t = new Tester(); import t._
      val i = \/-(user2.emailAddr)
      runResetPassword1(i)
      val token = db.prevToken()
      val p2 = PlainTextPassword("asdjhf2314sdfajk")

      "update the password when valid" - {
        assertEq(security.attemptLogin(i, p2).value, None)
        assertEq(runResetPassword2(Request(token, p2)), \/-(Result.Success))
        assertEq(security.attemptLogin(i, p2).value.isDefined, true)
      }

      "reject invalid passwords" -
        runResetPassword2(Request(token, PlainTextPassword("x"))).needLeft

      "reject invalid tokens" -
        assertEq(runResetPassword2(Request(SecurityToken("xxxx"), p2)), \/-(Result.TokenInvalid))

      "reject expired tokens" - {
        forwardTimeToEndOfPasswordResetWindow(Invalid)
        assertEq(runResetPassword2(Request(token, p2)), \/-(Result.TokenExpired))
      }
    }

  }
}
