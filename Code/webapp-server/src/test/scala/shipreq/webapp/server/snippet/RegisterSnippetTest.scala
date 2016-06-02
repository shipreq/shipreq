package shipreq.webapp.server.snippet

import net.liftweb.http.{ResponseShortcutException, S}
import net.liftweb.util.Helpers.intToTimeSpanBuilder
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken
import org.postgresql.util.PSQLException
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.taskman.api.Msg.RegistrationRequested
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.feature.validation.Validators
import shipreq.webapp.server.security.Oshiro
import shipreq.webapp.server.snippet.Register._
import shipreq.webapp.server.test.SnippetTestUtil._
import shipreq.webapp.server.test.WebappServerTestUtil._
import shipreq.webapp.server.test._

object RegisterSnippetTest extends TestSuite {

  lazy val reg1html = requireTemplate("public/register1")

  def assertSingleError(substring: String) {
    assertEq(S.errors.size, 1)
    assertContainsCI(S.errors.head._1.toString, substring)
  }

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

  override def tests = TestSuite {
    PrepareEnv()

    'isTokenExpired {
      'oneDay  - assertEq(isTokenExpired(1.day.ago), false)
      'oneWeek - assertEq(isTokenExpired(1.week.ago), true)
    }

    'Register1 {

      'render - {
        def test(config: Boolean, allowed: Boolean): Unit =
          inMockSession {
            val orig = ServerConfig.AllowRegister
            try {
              ServerConfig.AllowRegister = () => config
              val x = Register1.render(reg1html)
              val h = x.toString
              assertEq(h contains "register1Form", allowed)
              assertEq(h contains "registrationDisabled", !allowed)
            } finally {
              ServerConfig.AllowRegister = orig
            }
          }

        "allow registration to anonymous user when config on" - {
          test(true, true)
        }
        "deny registration to anonymous user when config off" - {
          test(false, false)
        }
        "deny registration to non-admin user when config off" - {
          UserFixture.Session(_.user2.withLoggedIn(test(false, false)))
        }
        "allow registration to admin even when config off" - {
          UserFixture.Session(_.user1.withLoggedIn(test(false, true)))
        }
      } // render

      'onSubmit {
        def test(dbu: DbUtil, email: String, usrTableDiff: Int) =
          withTestTaskman(
            dbu.assertRowCountChanges(DbTable.Usr -> usrTableDiff)(
              Register1.perform(Validators.email.correctAndValidateU(email))))

        def testSuccess(emailFn: UserFixture => String, usrTableDiff: Int, tokenChange: Boolean): Unit =
          UserFixture.Transaction { uf =>
            val dbu = uf.toDbUtil
            val email = emailFn(uf)
            val tokenBefore = dbu.lookupConfirmationToken(email)
            val (r, tt) = test(dbu, email, usrTableDiff)
            r.assertJsAlert(None)
            val token = dbu.lookupConfirmationToken(email)
            assert(token.isDefined)
            assert((token !=* tokenBefore) ==* tokenChange)
            SubmittedOneTask{ case RegistrationRequested(_,url) => () => assertContains(url, token.get) } test tt
          }

        "when email is invalid -- should reject request" - {
          val (r, tt) = TestDb.DbUtil(test(_, "not_an_email", 0))
          r.assertJsAlert(Some("Email"))
          NoTasksSubmitted.test(tt)
        }

        "when a pending, valid token exists -- should resend email" - {
          testSuccess(_.userWithCurrentToken.email.value, 0, false)
        }

        "when a pending, expired token exists -- should create a new token and email" - {
          testSuccess(_.userWithExpiredToken.email.value, 0, true)
        }

        "when a email is valid and new -- should create a user, token and send email" - {
          testSuccess(_ => "blarrr@yay.com", 1, true)
        }

        "when a email belongs to registered account -- should email with link to reset password" - {
          val (r, tt) = UserFixture.Transaction(uf => test(uf.toDbUtil, uf.user1.email.value, 0))
          r.assertJsAlert(None)
          SubmittedOneTask(ReRegistrationAttemptedT) test tt
        }
      } // onSubmit

    } // Register1

    'Register2 {

      'validateToken {
        def inEnv[A](f: UserFixture => A): A =
          inMockSession(UserFixture.Transaction(f))

        "redirect to Register1 with error when token is invalid" - {
          inEnv { _ =>
            intercept[ResponseShortcutException] {new Reg2Tester("blah").snippet.validateToken_!()}
            assertSingleError("invalid")
          }
        }

        "redirect to Register1 with error when token has expired" - {
          inEnv { uf =>
            intercept[ResponseShortcutException] {new Reg2Tester(uf.userWithExpiredToken.token).snippet.validateToken_!()}
            assertSingleError("expired")
          }
        }

        "render new-user form when token is valid" - {
          inEnv { uf =>
            new Reg2Tester(uf.userWithCurrentToken.token).snippet.validateToken_!()
            assert(S.errors.isEmpty)
          }
        }
      } // validateToken

      'POST {
        def inEnv[A](f: UserFixture => A): A =
          withOshiro(UserFixture.Transaction(f))

        def tester(uf: UserFixture) = {
          val t = new Reg2Tester(uf.userWithCurrentToken.token)
          t name_=      "John Stuff"
          t username_=  "crazy50"
          t password1_= "abcd5678"
          t password2_= "abcd5678"
          t tos_=       true
          t
        }

        'failure {
          def assertUnconfirmed(uf: UserFixture) {
            val reg = uf.toDbUtil.dao.findUserRegistrationInfo(uf.userWithCurrentToken.email).get
            assertEq(reg.confirmationSentAt, Some(uf.userWithCurrentToken.tokenCreatedAt))
            assertEq(reg.confirmationToken, Some(uf.userWithCurrentToken.token))
            assertEq(reg.confirmedAt, None)
          }

          def testFailure(mutate: Reg2Tester => Any)(uf: UserFixture) {
            val t = tester(uf)
            mutate(t)
            val js = t.onSubmitF()
            assertUnconfirmed(uf)
            js.assertJsAlert(Some(""))
          }

          "reject an invalid name" - {
            inEnv(testFailure(_ name_= "9000"))
          }

          "reject an invalid username" - {
            inEnv(testFailure(_ username_= "9000"))
          }

          "reject an invalid password" - {
            inEnv(testFailure(_ password2_= "abcd"))
          }

          "reject when passwords dont match" - {
            inEnv(testFailure(_ password1_= "987654321zcbsdfg"))
          }

          "reject a taken username" - {
            inEnv { uf =>
              val t = tester(uf)
              t username_= uf.user2.username.value
              t.onSubmitF()
              try {assertUnconfirmed(uf)}
              catch {case e: PSQLException if e.getMessage.contains("transaction is aborted") => }
            }
          }

          "reject without ToS agreement" - {
            inEnv(testFailure(_ tos_= false))
          }
        }

        'success {
          "create user" - {
            inEnv { uf =>
              tester(uf).onSubmit()
              val reg = uf.toDbUtil.dao.findUserRegistrationInfo(uf.userWithCurrentToken.email).get
              assertEq(reg.confirmationSentAt, Some(uf.userWithCurrentToken.tokenCreatedAt))
              assertEq(reg.confirmationToken, None)
              assertEq(reg.confirmedAt.get.isAfter(1.minute.ago.toMillis), true)
              assert(reg.confirmedAt.isDefined)

              val (user, pwd) = uf.toDbUtil.dao.findUserDescAndCredentials(uf.userWithCurrentToken.email.value).get
              assertEq(user.username.value, "crazy50")
              assert(pwd.hashedPassword.value !=* "abcd5678")
            }
          }

          "login" - {
            inEnv { uf =>
              assertNotLoggedIn()
              tester(uf).onSubmit()
              val user = assertLoggedIn()
              assertEq(user.username.value, "crazy50")
              assertEq(user.email, uf.userWithCurrentToken.email)
            }
          }

          "hash password so that login auth works with same plaintext password" - {
            inEnv { uf =>
              val subj = SecurityUtils.getSubject
              tester(uf).onSubmit()
              subj.logout()
              subj.login(new UsernamePasswordToken("crazy50", "abcd5678"))
            }
          }

          "hide the form and show the success" - {
            inEnv { uf =>
              val (js, _) = tester(uf).onSubmit()
              js.assertJsAlert(None)
              assertContains(js.toJsCmd, "toggle")
            }
          }

          "submit a msg to taskman" - {
            inEnv { uf =>
              val (_, tt) = tester(uf).onSubmit()
              SubmittedOneTask(RegistrationCompletedT) test tt
            }
          }
        }

      } // POST

    } // Register2
  }
}
