package shipreq.webapp.server.app

import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.server.app.AppSiteMap.Implicits._
import shipreq.webapp.server.app.AppSiteMap._

object AppSiteMapTest extends TestSuite {

  val homeRel = "/"
  val homeAbs = "http://localhost:8090"
  val loginRel = "/login"
  val loginAbs = "http://localhost:8090/login"

  override def tests = TestSuite {
    s"Home relativeUrl is $homeRel" - assertEq(Home.relativeUrl, homeRel)
    s"Home absoluteUrl is $homeAbs" - assertEq(Home.absoluteUrl, homeAbs)
    s"Login relativeUrl is $loginRel" - assertEq(Login.relativeUrl, loginRel)
    s"Login absoluteUrl is $loginAbs" - assertEq(Login.absoluteUrl, loginAbs)
  }
}
