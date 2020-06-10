package shipreq.webapp.server.app

import shipreq.base.test.BaseTestUtil._
import utest._
import shipreq.base.util.Url

object AnalyticsProxyTest extends TestSuite {

  private def test(prefix: String, fromTo: (String, String)): Unit = {
    val (from, to) = fromTo
    val actual = new AnalyticsProxy(Url.Absolute(prefix)).reRoute(Url.Absolute(from)).absoluteUrl
    assertEq(actual, to)
  }

  override def tests = Tests {
    "dev" - test("http://localhost:1234/", "https://www.google-analytics.com/analytics.js" -> "http://localhost:1234/www.google-analytics.com/analytics.js")
    "prodPath" - test("https://shipreq.com/ap/", "https://www.google-analytics.com/analytics.js" -> "https://shipreq.com/ap/www.google-analytics.com/analytics.js")
    "prodHost" - test("https://ap.shipreq.com", "https://www.google-analytics.com/analytics.js" -> "https://ap.shipreq.com/www.google-analytics.com/analytics.js")
  }
}