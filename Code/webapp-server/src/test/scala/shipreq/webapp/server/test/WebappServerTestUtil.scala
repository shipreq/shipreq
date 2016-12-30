package shipreq.webapp.server.test

import java.time._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken
import utest.asserts._
import shipreq.webapp.base.test.{WebappTestEquality, WebappTestUtil}
import shipreq.webapp.server.data.UserDescriptor
import shipreq.webapp.server.security.Oshiro

trait WebappServerTestEquality extends WebappTestEquality {
}

trait WebappServerTestUtil extends WebappTestUtil {
  import WebappServerTestUtil._

  def login(username: String, password: String): Unit =
    SecurityUtils.getSubject.login(new UsernamePasswordToken(username, password))

  def logout(): Unit =
    SecurityUtils.getSubject.logout()

  def withLoggedIn[A](username: String, password: String)(a: => A): A = {
    login(username, password)
    try a finally logout()
  }

  def withOshiro[A](a: => A): A = {
    PrepareEnv.oshiro()
    Oshiro.logout()
    try a
    finally Oshiro.logout()
  }

  def assertUserLoggedIn(u: UserDescriptor): Unit =
    assertEq(Oshiro.loggedInUser(), Some(u))

  def assertLoggedIn(): UserDescriptor = {
    val user = Oshiro.loggedInUser()
    assert(user.isDefined)
    user.get
  }

  def assertNotLoggedIn(): Unit = {
    val user = Oshiro.loggedInUser()
    assert(user.isEmpty)
  }

  implicit def toWSTU_IntExt(i: Int) = new WSTU_IntExt(i)
  implicit def toWSTU_DurationExt(d: Duration) = new WSTU_DurationExt(d)
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
}
