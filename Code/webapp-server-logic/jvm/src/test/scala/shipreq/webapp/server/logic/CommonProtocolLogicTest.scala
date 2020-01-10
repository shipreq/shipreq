package shipreq.webapp.server.logic

import scalaz.{-\/, \/, \/-}
import utest._
import shipreq.base.util._
import shipreq.webapp.base.protocol.CommonProtocols
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.user._

object CommonProtocolLogicTest extends TestSuite {

  private final case class Tester(mockInterpreters: MockInterpreters = new MockInterpreters) {
    import mockInterpreters._

    val session = Security.SessionToken.anonymous()

    def runLogin(i: CommonProtocols.Login.ajax.Req) = assertProtected(common.ajaxLogin(session)(i).value)

    db.users ::= user2
  }

  override def tests = Tests {

    'login {
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

      'badAccountU  - test(-\/(Username("nope")), user2password)(Deny, None)
      'badAccountE  - test(\/-(EmailAddr("w@w.com")), user2password)(Deny, None)
      'badPasswordU - test(-\/(user2.username), PlainTextPassword("qweoiru1234SDFG"))(Deny, None)
      'badPasswordE - test(\/-(user2.emailAddr), PlainTextPassword("qweoiru1234SDFG"))(Deny, None)
      'successU - test(-\/(user2.username), user2password)(Allow, Some(user2.token))
      'successE - test(\/-(user2.emailAddr), user2password)(Allow, Some(user2.token))
    }

    'feedback {
      import CommonProtocols.SubmitFeedback._
      implicit val t = Tester(); import t._
      import mockInterpreters._

      'noSession - {
        val req    = Request(UserInput("yo!"), Metadata(None, "a", "b", user2.username))
        val jwt    = Security.SessionToken.anonymous()
        val result = common.ajaxSubmitFeedback(jwt)(req).value
        assertEq(result, ((), Allow))
        taskman.assertSubmitted(1)
      }

      'hasSession - {
        val req    = Request(UserInput("yo!"), Metadata(None, "a", "b", Username("x")))
        val jwt    = Security.SessionToken.anonymous().login(user2.toUser)
        val result = common.ajaxSubmitFeedback(jwt)(req).value
        assertEq(result, ((), Allow))
        taskman.assertSubmitted(1)
      }

      'noUser - {
        val req    = Request(UserInput("yo!"), Metadata(None, "a", "b", Username("i don't exist")))
        val jwt    = Security.SessionToken.anonymous()
        val result = common.ajaxSubmitFeedback(jwt)(req).value
        assertEq(result, ((), Deny))
        taskman.assertSubmitted(0)
      }
    }

  }
}
