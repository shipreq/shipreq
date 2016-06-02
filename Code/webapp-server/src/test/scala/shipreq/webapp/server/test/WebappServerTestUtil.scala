package shipreq.webapp.server.test

import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken
import org.joda.time.DateTime
import utest.assert
import shipreq.webapp.base.test.{WebappTestEquality, WebappTestUtil}
import shipreq.webapp.server.data.UserDescriptor
import shipreq.webapp.server.security.Oshiro

trait WebappServerTestEquality extends WebappTestEquality {
  implicit def univEqJodaDateTime: UnivEq[DateTime] = UnivEq.force
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
}

object WebappServerTestUtil
  extends WebappServerTestEquality
     with WebappServerTestUtil
