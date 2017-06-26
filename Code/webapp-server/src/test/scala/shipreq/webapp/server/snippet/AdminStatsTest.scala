package shipreq.webapp.server.snippet

import org.scalatest.FunSuite
import org.scalatest.Matchers._
import shipreq.webapp.server.test.PrepareEnv
import shipreq.webapp.server.test.SnippetTestUtil._

class AdminStatsTest extends FunSuite {

  lazy val template = requireTemplate("admin-stats")

  test("Page should render without errors") {
    PrepareEnv.db()
    val html = AdminStats.render(template).toString
    html should not include(" class=\"err\">")
  }
}
