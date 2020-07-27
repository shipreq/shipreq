package shipreq.webapp.server.app

import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.Url
import utest._

object AnalyticsProxyTest extends TestSuite {

  override def tests = Tests {

    "masked" - {
      def test(prefix: String, fromTo: (String, String)): Unit = {
        val (from, to) = fromTo
        val actual = new AnalyticsProxy(Url.Absolute(prefix)).masked(from).absoluteUrl
        assertEq(actual, to)
      }

      val m = "*(xxxxx)*/*(yyyyy)*"

      "dev"      - test("http://localhost:1234/",  m -> s"http://localhost:1234/$m")
      "prodPath" - test("https://shipreq.com/ap/", m -> s"https://shipreq.com/ap/$m")
      "prodHost" - test("https://ap.shipreq.com",  m -> s"https://ap.shipreq.com/$m")
    }

    "reRoute" - {
      @nowarn
      def test(prefix: String, fromTo: (String, String)): Unit = {
        val (from, to) = fromTo
        val actual = new AnalyticsProxy(Url.Absolute(prefix)).reRoute(Url.Absolute(from)).absoluteUrl
        assertEq(actual, to)
      }

      "dev" - test("http://localhost:1234/", "https://www.google-analytics.com/analytics.js" -> "http://localhost:1234/www.google-analytics.com/analytics.js")
      "prodPath" - test("https://shipreq.com/ap/", "https://www.google-analytics.com/analytics.js" -> "https://shipreq.com/ap/www.google-analytics.com/analytics.js")
      "prodHost" - test("https://ap.shipreq.com", "https://www.google-analytics.com/analytics.js" -> "https://ap.shipreq.com/www.google-analytics.com/analytics.js")
    }
  }
}