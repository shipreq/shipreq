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
    "Home relativeUrl" - assertEq(Home.relativeUrl, homeRel)
    "Home absoluteUrl" - assertEq(Home.absoluteUrl, homeAbs)
    "Login relativeUrl" - assertEq(Login.relativeUrl, loginRel)
    "Login absoluteUrl" - assertEq(Login.absoluteUrl, loginAbs)
  }
}
