package shipreq.webapp.server.logic.impl

import shipreq.base.util._
import shipreq.taskman.api.Task
import shipreq.webapp.base.data._
import shipreq.webapp.client.public.{PublicSpaEntryPoint, PublicSpaProtocols}
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.server.logic.algebra.{DB, Security}
import shipreq.webapp.server.logic.config.ServerLogicConfig
import shipreq.webapp.server.logic.test.MockInterpreters
import utest._

object PublicSpaLogicTest extends TestSuite {

  private final case class Tester(mockInterpreters: MockInterpreters = new MockInterpreters) {
    import mockInterpreters._

    def withConfig(f: ServerLogicConfig => ServerLogicConfig): Tester =
      Tester(mockInterpreters.withConfig(f))

    val initData = PublicSpaEntryPoint.InitData(Allow, None, assetManifest)
    val session = Security.SessionToken.anonymous()

    def runRegister1(i: PublicSpaProtocols.Register1.ajax.Req) = assertProtected(publicSpa.ajaxRegister1(i).value)
    def runRegister2(i: PublicSpaProtocols.Register2.ajax.Req) = assertProtected(publicSpa.ajaxRegister2(session)(i).value)
    def runResetPassword1(i: PublicSpaProtocols.ResetPassword1.ajax.Req) = assertProtected(publicSpa.ajaxResetPassword1(i).value)
    def runResetPassword2(i: PublicSpaProtocols.ResetPassword2.ajax.Req) = assertProtected(publicSpa.ajaxResetPassword2(i).value)

    db.users ::= user2
  }

  val ea = EmailAddr("blah@test.com")

  def assertRegistrationEmailSent(emailAddr: EmailAddr = ea)(implicit t: Tester): Unit = {
    import t._, mockInterpreters._
    val m = taskman.assertLastSubmitted { case m: Task.RegistrationRequested => m }
    assertEq(m.email.value, emailAddr.value)
    assertContains(m.verifyEmailUrl, db.prevToken().value)
  }

  override def tests = Tests {

    "register1" - {
      implicit val t = Tester(); import t._, mockInterpreters._

      def runSuccessfully(tokensIssued: Int, msgsSubmitted: Int, emailAddr: EmailAddr = ea): Unit =
        db.assertIssuesTokens(tokensIssued)(
          taskman.assertSubmits(msgsSubmitted)(
            runRegister1(emailAddr).getOrThrow()))

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
        val m = taskman.assertLastSubmitted { case m: Task.ReRegistrationAttempted => m }
        assertEq(m.email.value, user2.emailAddr.value)
      }

      "email is invalid -- should reject request" - {
        runRegister1(EmailAddr("not_an_email")).getLeftOrThrow()
        taskman.assertSubmitted(0)
      }

      "registrationsOff" - {
        val t2 = t.withConfig(_.copy(publicRegistration = Deny))
        import t2._
        runRegister1(ea).getLeftOrThrow()
      }
    }

    "register2" - {
      import PublicSpaProtocols.Register2._

      def testSuccess(t: Tester, req: Request) = {
        import t._, mockInterpreters._
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
        assertEq(r, (\/-(Result.Success), Some(u.token).withSession(r._2).withoutExpiry))
        taskman.assertLastSubmitted { case _: Task.RegistrationCompleted => () }
      }

      val t = Tester(); import t._, mockInterpreters._

      // Mock user (pending)
      runRegister1(ea).getOrThrow()
      val token = db.prevToken()
      val req = Request(token, PersonName("Big Bob"), Username("bob"), PlainTextPassword("big_BOB_123!"), false)

      "success" - testSuccess(t, req)

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
        assertFailure(req.copy(personName = PersonName(""))).getLeftOrThrow()

      "reject an invalid username" -
        assertFailure(req.copy(username = Username("9000"))).getLeftOrThrow()

      "reject an invalid password" -
        assertFailure(req.copy(password = PlainTextPassword("abc"))).getLeftOrThrow()

      "reject a taken username" -
        assertEq(assertFailure(req.copy(username = user2.username)), \/-(Result.UsernameTaken))

      "registrationsOff" - {
        val t2 = t.withConfig(_.copy(publicRegistration = Deny))
        import t2._, mockInterpreters._
        db.userPlaceholders = Map(ea -> DB.UserRegistration.Pending(UserId(2), token, svr.clock))
        testSuccess(t2, req)
      }
    }

    "resetPassword1" - {
      def runSuccessfully(id: Username \/ EmailAddr, tokensIssued: Int, msgsSubmitted: Int)(implicit t: Tester): Unit = {
        import t._, mockInterpreters._
        db.assertIssuesTokens(tokensIssued)(
          taskman.assertSubmits(msgsSubmitted)(
            runResetPassword1(id)))
      }

      def assertResetPasswordEmailSent()(implicit t: Tester): Unit = {
        import t._, mockInterpreters._
        val m = taskman.assertLastSubmitted { case m: Task.PasswordResetRequested => m }
        assertEq(m.email.value, user2.emailAddr.value)
        assertContains(m.resetPasswordUrl, db.prevToken().value)
        assertEq(db.getUser(user2.pubids.head).get.resetPassword.map(_._1), Some(db.prevToken()))
      }

      "send email with new token when user found and doesn't have a reset-pw token yet" -
        List(0, 1).foreach { i =>
          implicit val t = Tester(); import t._, mockInterpreters._
          val u = user2.pubids(i)
          runSuccessfully(u, 1, 1)
          assertResetPasswordEmailSent()
        }

      "send email with current token when user found and valid reset-pw token exists" -
        List(0, 1).foreach { i =>
          implicit val t = Tester(); import t._, mockInterpreters._
          val u = user2.pubids(i)
          runSuccessfully(u, 1, 1)
          forwardTimeToEndOfPasswordResetWindow(Valid)
          runSuccessfully(u, 0, 1)
          assertResetPasswordEmailSent()
        }

      "send email with new token when user found and reset-pw token has expired" -
        List(0, 1).foreach { i =>
          implicit val t = Tester(); import t._, mockInterpreters._
          val u = user2.pubids(i)
          runSuccessfully(u, 1, 1)
          forwardTimeToEndOfPasswordResetWindow(Invalid)
          runSuccessfully(u, 1, 1)
          assertResetPasswordEmailSent()
        }


      "do nothing and seem ok when no user found" - {
        val t = Tester(); import t._, mockInterpreters._
        db.assertNoDbChange(
          assertDifference(taskman.msgs.length)(0) {
            runResetPassword1(-\/(Username("xxxxxxxxxxxxxx")))
            runResetPassword1(\/-(EmailAddr("xxxxxxxxx@xxxxx.com")))
          }
        )
      }

      "send registration email when user found and account not activated" - {
        implicit val t = Tester(); import t._, mockInterpreters._
        assertDifference(db.userPlaceholders.size)(1)(runRegister1(ea).getOrThrow())
        runSuccessfully(\/-(ea), 0, 1)
        assertRegistrationEmailSent()
      }
    }

    "resetPassword2" - {
      import PublicSpaProtocols.ResetPassword2._
      implicit val t = Tester(); import t._, mockInterpreters._
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
        runResetPassword2(Request(token, PlainTextPassword("x"))).getLeftOrThrow()

      "reject invalid tokens" -
        assertEq(runResetPassword2(Request(VerificationToken("xxxx"), p2)), \/-(Result.TokenInvalid))

      "reject expired tokens" - {
        forwardTimeToEndOfPasswordResetWindow(Invalid)
        assertEq(runResetPassword2(Request(token, p2)), \/-(Result.TokenExpired))
      }
    }

  }
}
