package shipreq.webapp.server.app

import shipreq.base.test.BaseTestUtil._
import utest._

object ServerInterpreterTest extends TestSuite {
  import ServerInterpreter._

  override def tests = Tests {
    "extractIpFromXForwardedFor" - {

      "single4" - assertEq(
        extractIpFromXForwardedFor("203.0.113.195"),
        "203.0.113.195")

      "proxy4" - assertEq(
        extractIpFromXForwardedFor("23.36.77.40, 46.105.99.163"),
        "23.36.77.40")

      "single6" - assertEq(
        extractIpFromXForwardedFor("2001:db8:85a3:8d3:1319:8a2e:370:7348"),
        "2001:db8:85a3:8d3:1319:8a2e:370:7348")

      "single6" - assertEq(
        extractIpFromXForwardedFor("2001:db8:85a3:8d3:1319:8a2e:370:7348"),
        "2001:db8:85a3:8d3:1319:8a2e:370:7348")

      "proxy6" - assertEq(
        extractIpFromXForwardedFor("0:0:0:0:0:0:0:1, 2001:db8:85a3:8d3:1319:8a2e:370:7348"),
        "0:0:0:0:0:0:0:1")
    }
  }
}
