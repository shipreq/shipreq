package shipreq.webapp.server.test

import java.time._
import org.apache.shiro.codec.Base64
import org.apache.shiro.util.ByteSource
import utest.asserts._
import shipreq.webapp.base.test.{WebappTestEquality, WebappTestUtil}
import shipreq.webapp.base.user.{PlainTextPassword, User, Username}
import shipreq.webapp.server.logic.PasswordAndSalt
import shipreq.webapp.server.security.AppSecurityRealm

trait WebappServerTestEquality extends WebappTestEquality {
}

trait WebappServerTestUtil extends WebappTestUtil {
  import WebappServerTestUtil._
  import PrepareEnv.security

  def login(usernameOrEmail: String, password: PlainTextPassword): Unit =
    security.attemptLogin(Username.orEmail(usernameOrEmail), password).unsafePerformIO() match {
      case Some(_) => ()
      case None    => sys error s"Login failed for [$usernameOrEmail] [$password]"
    }

  def logout(): Unit =
    security.logout.unsafePerformIO()

  def withLoggedIn[A](username: Username, password: PlainTextPassword)(a: => A): A = {
    login(username.value, password)
    try a finally logout()
  }

  def withShiro[A](a: => A): A = {
    PrepareEnv.shiro()
    logout()
    try a finally logout()
  }

  def assertUserLoggedIn(u: User): Unit =
    assertEq(security.authenticatedUser.unsafePerformIO(), Some(u))

  def assertLoggedIn(): User = {
    val user = security.authenticatedUser.unsafePerformIO()
    assert(user.isDefined)
    user.get
  }

  def assertNotLoggedIn(): Unit = {
    val user = security.authenticatedUser.unsafePerformIO()
    assert(user.isEmpty)
  }

  implicit def toWSTU_IntExt(i: Int) = new WSTU_IntExt(i)
  implicit def toWSTU_DurationExt(d: Duration) = new WSTU_DurationExt(d)
  implicit def PasswordAndSaltExt(d: PasswordAndSalt) = new PasswordAndSaltExt(d)
}

object WebappServerTestUtil
  extends WebappServerTestEquality
     with WebappServerTestUtil {

  class WSTU_IntExt(private val i: Int) extends AnyVal {
    def second  = Duration.ofSeconds(i)
    def seconds = Duration.ofSeconds(i)
    def minute  = Duration.ofMinutes(i)
    def minutes = Duration.ofMinutes(i)
    def hour    = Duration.ofHours(i)
    def hours   = Duration.ofHours(i)
    def day     = Duration.ofDays(i)
    def days    = Duration.ofDays(i)
    def week    = Duration.ofDays(i * 7)
    def weeks   = Duration.ofDays(i * 7)
  }

  class WSTU_DurationExt(private val d: Duration) extends AnyVal {
    def ago: Instant = Instant.now().minus(d)
  }

  class PasswordAndSaltExt(private val ps: PasswordAndSalt) extends AnyVal {
    def matches(p: PlainTextPassword): Boolean = {
      val saltBytes = ByteSource.Util.bytes(Base64.decode(ps.salt.base64))
      val hash2 = AppSecurityRealm.pureHashFn(p, saltBytes)
      ps.passwordHash ==* hash2.passwordHash
    }
  }
}
