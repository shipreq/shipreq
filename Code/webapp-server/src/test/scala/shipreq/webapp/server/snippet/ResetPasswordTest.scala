package shipreq.webapp.server.snippet

import doobie.imports._
import java.time._
import utest._
import shipreq.taskman.api.UserId
import shipreq.webapp.server.data.{ResetPasswordInfo, UserRegistrationInfo}
import shipreq.webapp.server.db.DbLogic
import shipreq.webapp.server.feature.validation.Validators
import shipreq.webapp.server.test.SnippetTestUtil._
import shipreq.webapp.server.test.WebappServerTestUtil._
import shipreq.webapp.server.test._

object ResetPasswordTest extends TestSuite {

  val validEmail = "hehe@yay.com"
  val expiredTime = 2 days
  val nonExpiredTime = 1 hour

  lazy val template2 = requireTemplate("public/resetpw2")

  override def tests = TestSuite {

    "ResetPassword1.perform" - {

      def registeredUser = UserRegistrationInfo(UserId(5), None, None, Some(Instant.now()))
      def existingToken(token: String, age: Duration) = ResetPasswordInfo(Some(token), Some(age.ago))
      def noResetPwToken = ResetPasswordInfo(None, None)

      sealed abstract class DbSetup(val state: Option[(UserRegistrationInfo, ResetPasswordInfo)])
      object UserNotFound             extends DbSetup(None)
      object UnactivatedUser          extends DbSetup(Some(UserRegistrationInfo(UserId(5), Some("X"), Some(1.minute.ago), None), noResetPwToken))
      object UserWithoutExistingToken extends DbSetup(Some(registeredUser, noResetPwToken))
      object UserWithExpiredToken     extends DbSetup(Some(registeredUser, existingToken("EXPIRED", expiredTime)))
      object UserWithValidToken       extends DbSetup(Some(registeredUser, existingToken("VALID", nonExpiredTime)))

      sealed abstract class DbExp(val newToken: Boolean, val reuse: Boolean)
      case object NoDbChange     extends DbExp(false, false)
      case object IssuesNewToken extends DbExp(true, false)
      case object ReusesToken    extends DbExp(false, true)

      val PasswordResetTaskSubmitted   = TestTaskman.SubmittedOneTask(TestTaskman.PasswordResetRequested)
      val ConfirmRegistrationEmailSent = TestTaskman.SubmittedOneTask(TestTaskman.RegistrationRequested("http"))

      val JsEmailSent     = NoErrorNotice & JsContains("resetpwTokenSent")
      val JsEmailRejected = HasErrorNotice("mail address") & JsDoesntContain("resetpwTokenSent")

      def test(emailInput: String, dbSetup: DbSetup)(dbExp: DbExp, taskExp: TestTaskman.Exp, jsExp: JsExp): Unit =
        TestDb(inTransaction = false).runNow { xa =>

          for ((ur, rp) <- dbSetup.state) {
            val resetCount = rp.token.size
            ur.confirmedAt match {
              case None =>
                xa ! sql"""
                    INSERT INTO usr(
                      id, email,
                      confirmation_token, confirmation_sent_at, confirmed_at,
                      reset_password_token, reset_password_sent_at, reset_password_req_count
                    )
                    VALUES(
                      ${ur.id.value}, $emailInput,
                      ${ur.confirmationToken}, ${ur.confirmationSentAt}, NULL,
                      ${rp.token}, ${rp.sentAt}, $resetCount
                    )
                  """.update.run
              case Some(_) =>
                xa ! sql"""
                    INSERT INTO usr(
                      id, email, username,
                      password, password_salt, password_changed_at,
                      confirmation_token, confirmation_sent_at, confirmed_at,
                      reset_password_token, reset_password_sent_at, reset_password_req_count
                    )
                    VALUES(
                      ${ur.id.value}, $emailInput, 'blah',
                      '', '', NOW(),
                      ${ur.confirmationToken}, ${ur.confirmationSentAt}, ${ur.confirmedAt},
                      ${rp.token}, ${rp.sentAt}, $resetCount
                    )
                  """.update.run
            }
          }

          def dbInspect(): List[(Int, Option[String])] =
            xa ! sql"SELECT reset_password_req_count,reset_password_token FROM usr".query[(Int,Option[String])].list

          val dbBefore = dbInspect()
          val (jsCmd, tt) = TestTaskman.use(
            ResetPassword1.perform(Validators.email.correctAndValidateU(emailInput))
          )
          val dbAfter = dbInspect()

          assertEq(dbAfter.size, dbBefore.size)
          dbExp match {
            case NoDbChange =>
              assertEq(dbAfter, dbBefore)
            case IssuesNewToken =>
              assertEq(dbAfter.size, 1)
              val (reqCount1, token1) = dbBefore.head
              val (reqCount2, token2) = dbAfter.head
              assertEq(reqCount2, reqCount1 + 1)
              assert(token2 !=* token1, token2.isDefined)
            case ReusesToken =>
              assertEq(dbAfter.size, 1)
              val (reqCount1, token1) = dbBefore.head
              val (reqCount2, token2) = dbAfter.head
              assertEq(reqCount2, reqCount1 + 1)
              assertEq(token2, token1)
              assert(token2.isDefined)
          }
          jsExp.test(jsCmd)
          taskExp.test(tt)
        }

      "do nothing and pretend email sent when no user found and email is valid" -
        test(validEmail, UserNotFound)(NoDbChange, TestTaskman.NoTasksSubmitted, JsEmailSent)

      "reject email address when no user found and email is invalid" -
        test("invalidEmail", UserNotFound)(NoDbChange, TestTaskman.NoTasksSubmitted, JsEmailRejected)

      "send registration email when user found and account not activated" -
        test(validEmail, UnactivatedUser)(NoDbChange, ConfirmRegistrationEmailSent, JsEmailSent)

      "send email with current token when user found and valid reset-pw token exists" -
        test(validEmail, UserWithValidToken)(ReusesToken, PasswordResetTaskSubmitted, JsEmailSent)

      "send email with new token when user found and reset-pw token has expired" -
        test(validEmail, UserWithExpiredToken)(IssuesNewToken, PasswordResetTaskSubmitted, JsEmailSent)

      "send email with new token when user found and doesnt have a reset-pw token yet" -
        test(validEmail, UserWithoutExistingToken)(IssuesNewToken, PasswordResetTaskSubmitted, JsEmailSent)
    }

    "ResetPassword2.render" - {
      def test(tokenAge: Option[Instant])(ne: SNoticeExp, re: RenderExp): Unit =
        inLiftSession(
          TestDb(inTransaction = false).runNow { xa =>
            val token = "ah"
            for (age <- tokenAge)
              xa ! sql"""
                  INSERT INTO usr(
                    email, username, password, password_salt, password_changed_at, confirmed_at,
                    login_count, last_login_at, last_login_ip,
                    reset_password_token, reset_password_sent_at, reset_password_req_count)
                  VALUES(
                    '','','','',NOW(),NOW(),
                    1,NOW(),'',
                    $token, $age, 1
                  )
                """.update.run
            val s = new ResetPassword2(token)
            val r = tryRender(s.render(template2))
            ne.test()
            re.test(r)
          }
        )

      "redirect when token not found" -
        test(None)(HasErrorNoticeContaining("invalid"), Redirects)

      "redirect when token expired" -
        test(Some(expiredTime.ago))(HasErrorNoticeContaining("expired"), Redirects)

      "provide form when token valid" -
        test(Some(nonExpiredTime.ago))(NoNotices, HtmlContains("<form "))
    }

    "ResetPassword2.onSubmit" - {
      def test(p: String)(reject: Boolean, jsExp: JsExp): Unit =
        inLiftSession(
          UserFixture.Session.runNow { uf =>
            import uf._
            val token = "ah"
            xa ! DbLogic.user.performInstallNewResetPasswordToken(user1.id, () => token)
            def passwordGet(): String = xa ! sql"SELECT password FROM usr WHERE id=${user1.id.value}".query[Option[String]].unique.map(_.get)
            val password1 = passwordGet()
            val s = new ResetPassword2(token)
            s.vars = (p, p)
            val js = s.onSubmit()
            val password2 = passwordGet()
            assert((password1 ==* password2) ==* reject)
            jsExp test js
          }
        )

      "reject invalid passwords" - test("x")(true, HasErrorNotice("assword"))
      "update the password when valid" - test("asdjhf2314sdfajk")(false, JsContains("toggle"))
    }

  }
}
