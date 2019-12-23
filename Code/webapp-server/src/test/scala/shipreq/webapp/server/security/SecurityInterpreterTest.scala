package shipreq.webapp.server.security

import java.time.Duration
import scalaz.{-\/, Name}
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.server.logic.{MockInterpreters, SimpleEndpoints}
import shipreq.webapp.server.logic.dispatch.Cookie
import shipreq.webapp.client.public.PublicSpaProtocols.Login
import shipreq.webapp.server.ServerLogicConfig.Security.JwtSecret
import shipreq.webapp.server.logic.Security.{SessionRestoreResult, SessionToken}

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
      assertEq(sec.sessionRestore(cookieJar).value, SessionRestoreResult.None)
      val s1 = sec.sessionRestoreOrCreate(cookieJar).value
      sessionPersist(s1)
      assert(s1.sessionId.isDefined)
      assertEq(s1.sessionId.map(_.value.length), Some(36))
      assertEq(s1.authenticatedUser, None)
      assertEq(sec.sessionRestore(cookieJar).value, SessionRestoreResult.Success(s1))
      assertEq(sec.sessionRestoreOrCreate(cookieJar).value, s1)

      val s2 = loginUser2(s1)
      assertEq(s2.sessionId, s1.sessionId)
      assertEq(s2.authenticatedUser, Some(user2.toUser))
      sessionPersist(s2)
      assertEq(sec.sessionRestore(cookieJar).value, SessionRestoreResult.Success(s2))
      assertEq(sec.sessionRestoreOrCreate(cookieJar).value, s2)

      val s3 = s2.logout
      assertEq(s3.sessionId, s2.sessionId)
      assertEq(s3.authenticatedUser, None)
      logout()
      assertEq(sec.sessionRestore(cookieJar).value, SessionRestoreResult.Success(s3))
      assertEq(sec.sessionRestoreOrCreate(cookieJar).value, s3)

      s1.sessionId
    }

    "migration" - {
      val s0 = SessionToken(None, Some(user2.toUser))
      val List(s1, s2) = List((), ()).map { _ =>
        sessionPersist(s0)
        val o = sec.sessionRestore(cookieJar).value
        assertMatch(o) {
          case SessionRestoreResult.Success(t) if t.sessionId.isEmpty =>
        }
        val s = sec.sessionRestoreOrCreate(cookieJar).value
        assert(s.sessionId.isDefined)
        s
      }
      assertEq(s1.copy(sessionId = None), s0)
      assertEq(s2.copy(sessionId = None), s0)
      assertNotEq(s1.sessionId.get, s2.sessionId.get)
    }

    "secretRotation" - {
      val secret1 = new JwtSecret("1" * 64)
      val sec1 = {
        implicit def config = mocks.config.security.copy(jwtSecretPrevious = None, jwtSecret = secret1)
        new SecurityInterpreter[Name]
      }

      val secret2 = new JwtSecret("2" * 64)
      val sec2 = {
        implicit def config = mocks.config.security.copy(jwtSecretPrevious = Some(secret1), jwtSecret = secret2)
        new SecurityInterpreter[Name]
      }

      val s = sec.sessionRestoreOrCreate(cookieJar).value
      assert(s.sessionId.isDefined)
      sessionPersist(s)(sec1)
      assertEq(sec1.sessionRestore(cookieJar).value, SessionRestoreResult.Success(s))
      assertEq(sec2.sessionRestore(cookieJar).value, SessionRestoreResult.Success(s))

      sessionPersist(s)(sec2)
      assertEq(sec2.sessionRestore(cookieJar).value, SessionRestoreResult.Success(s))
      assertEq(sec1.sessionRestore(cookieJar).value, SessionRestoreResult.None)
    }

    "expired" - {
      implicit val sec = {
        implicit def config = mocks.config.security.copy(jwtLifespan = Duration.ofMillis(1))
        new SecurityInterpreter[Name]
      }

      val s1 = loginUser2(SessionToken.anonymous())
      assert(s1.sessionId.isDefined)
      sessionPersist(s1)
      Thread.sleep(4)
      assertEq(sec.sessionRestore(cookieJar).value, SessionRestoreResult.Expired(s1))

      val s2 = sec.sessionRestoreOrCreate(cookieJar).value
      assertEq(s2.sessionId, s1.sessionId)
      assertEq(s2.authenticatedUser, None)
    }
  }
}
