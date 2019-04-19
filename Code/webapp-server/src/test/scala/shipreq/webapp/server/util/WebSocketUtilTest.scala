package shipreq.webapp.server.util

import shipreq.webapp.server.logic.Cookie
import shipreq.base.test.BaseTestUtil._
import utest._

object WebSocketUtilTest extends TestSuite {

  override def tests = Tests {

    'cookieLookup - {
      val headerValue = "_ga=GA1.1.1413438204.1554534657; _gid=GA1.1.2052487085.1555631496; JSESSIONID=node09adphhuh82ewyhdunwyz75880.node0; _gat=1; jwt=eyJhbGciOiJIUzUxMiJ9.eyJleHAiOjE1NTU3MTgxMTksInVpZCI6Inc1R3YiLCJzdWIiOiJnb2xseSJ9.vmPFU_tsCzgC8DCAOPp6NXwqrgRtXoSTtCzLJIyZw5utO13dgaDql2FE1WwZwQiHkSHWnJYBL0VvH2SFZDsHxw"
      val c = WebSocketUtil.cookieLookupFnOverHeader(headerValue)

      def test(name: String, expect: Option[String]): Unit =
        assertEq(c(Cookie.Name(name)), expect)

      'head - test("_ga", Some("GA1.1.1413438204.1554534657"))
      'mid1 - test("_gid", Some("GA1.1.2052487085.1555631496"))
      'mid2 - test("JSESSIONID", Some("node09adphhuh82ewyhdunwyz75880.node0"))
      'mid3 - test("_gat", Some("1"))
      'last - test("jwt", Some("eyJhbGciOiJIUzUxMiJ9.eyJleHAiOjE1NTU3MTgxMTksInVpZCI6Inc1R3YiLCJzdWIiOiJnb2xseSJ9.vmPFU_tsCzgC8DCAOPp6NXwqrgRtXoSTtCzLJIyZw5utO13dgaDql2FE1WwZwQiHkSHWnJYBL0VvH2SFZDsHxw"))
      'bad1 - test("__ga", None)
      'bad2 - test("_g", None)
      'bad3 - test("ga", None)
    }

  }
}
