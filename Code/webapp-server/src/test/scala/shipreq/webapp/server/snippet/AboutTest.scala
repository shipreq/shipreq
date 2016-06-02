package shipreq.webapp.server.snippet

import org.scalatest.FunSuite
import org.scalatest.Matchers._
import shipreq.webapp.server.test.SnippetTestUtil._

class AboutTest extends FunSuite {

  lazy val template = requireTemplate("about")

  test("Page should render without errors") {
    val html = (new About).attribution(template).toString
    countOccurrences(html, "©") should be > 10
  }
}
