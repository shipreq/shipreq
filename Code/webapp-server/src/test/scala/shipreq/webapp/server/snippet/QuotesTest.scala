package shipreq.webapp.server.snippet

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class QuotesTest extends FunSuite {

  test("All quotes should be single elem blockquotes") {
    for (q <- Quotes.quotes) {
      q.toString should (startWith("<blockquote") and endWith("</blockquote>"))
    }
  }
}
