package shipreq.webapp.server.logic

import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.ajax.CommonProtocols
import shipreq.webapp.base.test.WebappTestUtil._
import utest._

object CommonProtocolLogicTest extends TestSuite {

  private final case class Tester(mockInterpreters: MockInterpreters = new MockInterpreters) {
    import mockInterpreters._

    val session = Security.SessionToken.anonymous()

    def runLogin(i: CommonProtocols.Login.ajax.Req) = assertProtected(common.ajaxLogin(session)(i).value)

    db.users ::= user2
  }

  override def tests = Tests {
    import CommonProtocols.Metadata

    "login" - {
      import CommonProtocols.Login._
      implicit val t = Tester(); import t._
      import mockInterpreters._

      def test(usernameOrEmail: Username \/ EmailAddr, password: PlainTextPassword)
              (expectResp: Permission, expectToken: Option[Security.SessionToken[Any]]) =
        assertDifference("usrLoginLog", db.usrLoginLog.length)(if (expectResp is Allow) 1 else 0) {
          val r = runLogin(Request(usernameOrEmail, password))
          assertEq(r, (expectResp, expectToken.withSession(r._2).withoutExpiry))
          svr.runForked()
        }

      "badAccountU"  - test(-\/(Username("nope")), user2password)(Deny, None)
      "badAccountE"  - test(\/-(EmailAddr("w@w.com")), user2password)(Deny, None)
      "badPasswordU" - test(-\/(user2.username), PlainTextPassword("qweoiru1234SDFG"))(Deny, None)
      "badPasswordE" - test(\/-(user2.emailAddr), PlainTextPassword("qweoiru1234SDFG"))(Deny, None)
      "successU" - test(-\/(user2.username), user2password)(Allow, Some(user2.token))
      "successE" - test(\/-(user2.emailAddr), user2password)(Allow, Some(user2.token))
    }

    "reportClientError" - {
      import CommonProtocols.ReportClientError._
      implicit val t = Tester(); import t._
      import mockInterpreters._

      val err = ErrorInfo("name", "message", "", "asd\ndef", Map("omg.qwe" -> "asdf"))

      def test(req: Request, jwt: Security.SessionToken[Any], expectOk: Boolean): Unit = {
        val result = common.ajaxReportClientError(jwt)(req).value
        if (expectOk) {
          assertEq(result, ((), Allow))
          taskman.assertSubmitted(1)
        } else {
          assertEq(result, ((), Deny))
          taskman.assertSubmitted(0)
        }
      }

      "noUserNoSession" - {
        val req = Request(err, Metadata.Client(None, "a", "b", None))
        val jwt = Security.SessionToken.anonymous()
        test(req, jwt, true)
      }

      "userNoSession" - {
        val req = Request(err, Metadata.Client(None, "a", "b", Some(user2.username)))
        val jwt = Security.SessionToken.anonymous()
        test(req, jwt, true)
      }

      "userWithSession" - {
        val req = Request(err, Metadata.Client(None, "a", "b", Some(Username("x"))))
        val jwt = Security.SessionToken.anonymous().login(user2.toUser)
        test(req, jwt, true)
      }

      "noUserWithSession" - {
        val req = Request(err, Metadata.Client(None, "a", "b", None))
        val jwt = Security.SessionToken.anonymous().login(user2.toUser)
        test(req, jwt, true)
      }

      "badUserNoSession" - {
        val req = Request(err, Metadata.Client(None, "a", "b", Some(Username("i don't exist"))))
        val jwt = Security.SessionToken.anonymous()
        test(req, jwt, false)
      }

      "badUserWithSession" - {
        val req = Request(err, Metadata.Client(None, "a", "b", Some(Username("i don't exist"))))
        val jwt = Security.SessionToken.anonymous().login(user2.toUser)
        test(req, jwt, true)
      }
    }

    "submitFeedback" - {
      import CommonProtocols.SubmitFeedback._
      implicit val t = Tester(); import t._
      import mockInterpreters._

      "noSession" - {
        val req    = Request(UserInput("yo!"), Metadata.Client(None, "a", "b", Some(user2.username)))
        val jwt    = Security.SessionToken.anonymous()
        val result = common.ajaxSubmitFeedback(jwt)(req).value
        assertEq(result, ((), Allow))
        taskman.assertSubmitted(1)
      }

      "hasSession" - {
        val req    = Request(UserInput("yo!"), Metadata.Client(None, "a", "b", Some(Username("x"))))
        val jwt    = Security.SessionToken.anonymous().login(user2.toUser)
        val result = common.ajaxSubmitFeedback(jwt)(req).value
        assertEq(result, ((), Allow))
        taskman.assertSubmitted(1)
      }

      "badUser" - {
        val req    = Request(UserInput("yo!"), Metadata.Client(None, "a", "b", Some(Username("i don't exist"))))
        val jwt    = Security.SessionToken.anonymous()
        val result = common.ajaxSubmitFeedback(jwt)(req).value
        assertEq(result, ((), Deny))
        taskman.assertSubmitted(0)
      }

      "noUser" - {
        val req    = Request(UserInput("yo!"), Metadata.Client(None, "a", "b", None))
        val jwt    = Security.SessionToken.anonymous()
        val result = common.ajaxSubmitFeedback(jwt)(req).value
        assertEq(result, ((), Deny))
        taskman.assertSubmitted(0)
      }
    }

  }
}
