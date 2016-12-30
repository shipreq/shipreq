package shipreq.webapp.server.snippet.sir

import org.scalatest.FunSuite
import org.scalatest.Matchers._
import shipreq.webapp.server.test.PrepareEnv
import shipreq.webapp.server.test.SnippetTestUtil._

class StatsTest extends FunSuite {

  lazy val template = requireTemplate("sir/stats")

  test("Page should render without errors") {
    PrepareEnv.db()
    val html = Stats.render(template).toString
    html should not include(" class=\"err\">")
  }
}
