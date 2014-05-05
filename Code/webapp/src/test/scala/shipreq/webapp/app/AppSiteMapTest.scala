package shipreq.webapp.app

import org.scalatest.FunSuite
import shipreq.webapp.test.TestHelpers
import AppSiteMap._
import Implicits._

class AppSiteMapTest extends FunSuite with TestHelpers {

  val homeRel = "/"
  val homeAbs = "http://localhost:8090"
  val loginRel = "/login"
  val loginAbs = "http://localhost:8090/login"

  test(s"Home relativeUrl is $homeRel") {
    Home.relativeUrl shouldBe homeRel
  }

  test(s"Home absoluteUrl is $homeAbs") {
    Home.absoluteUrl shouldBe homeAbs
  }

  test(s"Login relativeUrl is $loginRel") {
    Login.relativeUrl shouldBe loginRel
  }

  test(s"Login absoluteUrl is $loginAbs") {
    Login.absoluteUrl shouldBe loginAbs
  }
}
