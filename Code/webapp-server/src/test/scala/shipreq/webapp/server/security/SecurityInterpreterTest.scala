package shipreq.webapp.server.security

import java.time.Duration
import scalaz.{-\/, Name}
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.protocol.ajax.CommonProtocols.Login
import shipreq.webapp.server.logic.{MockInterpreters, SimpleEndpoints}
import shipreq.webapp.server.logic.dispatch.Cookie
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
    import mocks.{common, db, trace, user2, user2password}

    db.users ::= user2
    implicit val sec = new SecurityInterpreter[Name]

    def loginUser2(s: SessionToken[Any]): SessionToken[Unit] =
      common.ajaxLogin(s)(Login.Request(-\/(user2.username), user2password)).value._2.get

    def sessionPersist(s: SessionToken[Any])(implicit sec: SecurityInterpreter[Name]): Unit =
      cookieJar.update(sec.sessionPersist(s).value)

    def logout()(implicit sec: SecurityInterpreter[Name]): Unit =
      cookieJar.update(SimpleEndpoints.logout(cookieJar).value)

    "standard" - {
      assertEq(sec.sessionRestore(cookieJar).value, SessionRestoreResult.None)
      val s1 = sec.sessionRestoreOrCreate(cookieJar).value
      sessionPersist(s1)
      assertEq(s1.sessionId.value.length, 36)
      assertEq(s1.authenticatedUser, None)
      assertEq(sec.sessionRestore(cookieJar).value.modToken(_.withoutExpiry), SessionRestoreResult.Success(s1.withoutExpiry))

      val s2 = loginUser2(s1)
      assertEq(s2.sessionId, s1.sessionId)
      assertEq(s2.authenticatedUser, Some(user2.toUser))
      sessionPersist(s2)
      assertEq(sec.sessionRestore(cookieJar).value.modToken(_.withoutExpiry), SessionRestoreResult.Success(s2))

      val s3 = s2.logout
      assertEq(s3.sessionId, s2.sessionId)
      assertEq(s3.authenticatedUser, None)
      logout()
      assertEq(sec.sessionRestore(cookieJar).value.modToken(_.withoutExpiry), SessionRestoreResult.Success(s3))

      s1.sessionId
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

      val s = sec.sessionRestoreOrCreate(cookieJar).value.withoutExpiry
      sessionPersist(s)(sec1)
      assertEq(sec1.sessionRestore(cookieJar).value.modToken(_.withoutExpiry), SessionRestoreResult.Success(s))
      assertEq(sec2.sessionRestore(cookieJar).value.modToken(_.withoutExpiry), SessionRestoreResult.Success(s))

      sessionPersist(s)(sec2)
      assertEq(sec2.sessionRestore(cookieJar).value.modToken(_.withoutExpiry), SessionRestoreResult.Success(s))
      assertEq(sec1.sessionRestore(cookieJar).value.modToken(_.withoutExpiry), SessionRestoreResult.None)
    }

    "expired" - {
      implicit val config = mocks.config.security.copy(jwtLifespan = Duration.ZERO)
      implicit val sec = new SecurityInterpreter[Name]

      val s1 = loginUser2(SessionToken.anonymous())
      sessionPersist(s1)
      assertEq(sec.sessionRestore(cookieJar).value.modToken(_.withoutExpiry), SessionRestoreResult.Expired(s1))

      val s2 = sec.sessionRestoreOrCreate(cookieJar).value
      assertEq(s2.sessionId, s1.sessionId)
      assertEq(s2.authenticatedUser, None)
    }
  }
}
