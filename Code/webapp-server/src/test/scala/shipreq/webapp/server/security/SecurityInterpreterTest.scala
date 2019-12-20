package shipreq.webapp.server.security

import scalaz.{-\/, Name}
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.server.logic.{MockInterpreters, SimpleEndpoints}
import shipreq.webapp.server.logic.dispatch.Cookie
import shipreq.webapp.client.public.PublicSpaProtocols.Login
import shipreq.webapp.server.logic.Security.SessionToken

object SecurityInterpreterTest extends TestSuite {

  final class CookieJar extends Cookie.LookupFn {
    private var cookies = Map.empty[Cookie.Name, Cookie]

    override def apply(name: Cookie.Name): Option[String] =
      cookies.get(name).map(_.value)

    def update(u: Cookie.Update): Unit = {
      cookies --= u.remove
      cookies ++= u.add.map(c => c.name -> c)
    }

    def modify(f: Cookie => Cookie): Unit =
      cookies = cookies.valuesIterator.map(f).map(c => c.name -> c).toMap
  }

  override def tests = Tests {

    val cookieJar = new CookieJar
    val mocks = new MockInterpreters()
    implicit def config = mocks.config.security
    import mocks.{db, publicSpa, trace, user2, user2password}

    db.users ::= user2
    implicit val sec = new SecurityInterpreter[Name]

    def loginUser2(s: SessionToken): SessionToken =
      publicSpa.ajaxLogin(s)(Login.Request(-\/(user2.username), user2password)).value._2.get

    def sessionPersist(s: SessionToken)(implicit sec: SecurityInterpreter[Name]): Unit =
      cookieJar.update(sec.sessionPersist(s).value)

    def logout()(implicit sec: SecurityInterpreter[Name]): Unit =
      cookieJar.update(SimpleEndpoints.logout(cookieJar).value)

    "standard" - {
      assertEq(sec.sessionRestore(cookieJar).value, None)
      val s1 = sec.sessionRestoreOrCreate(cookieJar).value
      sessionPersist(s1)
      assert(s1.sessionId.isDefined)
      assertEq(s1.sessionId.map(_.value.length), Some(36))
      assertEq(s1.authenticatedUser, None)
      assertEq(sec.sessionRestore(cookieJar).value, Some(s1))
      assertEq(sec.sessionRestoreOrCreate(cookieJar).value, s1)

      val s2 = loginUser2(s1)
      assertEq(s2.sessionId, s1.sessionId)
      assertEq(s2.authenticatedUser, Some(user2.toUser))
      sessionPersist(s2)
      assertEq(sec.sessionRestore(cookieJar).value, Some(s2))
      assertEq(sec.sessionRestoreOrCreate(cookieJar).value, s2)

      val s3 = s2.logout
      assertEq(s3.sessionId, s2.sessionId)
      assertEq(s3.authenticatedUser, None)
      logout()
      assertEq(sec.sessionRestore(cookieJar).value, Some(s3))
      assertEq(sec.sessionRestoreOrCreate(cookieJar).value, s3)

      s1.sessionId
    }

    "migration" - {
      val s0 = SessionToken(None, Some(user2.toUser))
      val List(s1, s2) = List((), ()).map { _ =>
        sessionPersist(s0)
        val o = sec.sessionRestore(cookieJar).value
        assert(o.exists(_.sessionId.isEmpty))
        val s = sec.sessionRestoreOrCreate(cookieJar).value
        assert(s.sessionId.isDefined)
        s
      }
      assertEq(s1.copy(sessionId = None), s0)
      assertEq(s2.copy(sessionId = None), s0)
      assertNotEq(s1.sessionId.get, s2.sessionId.get)
    }
  }
}
