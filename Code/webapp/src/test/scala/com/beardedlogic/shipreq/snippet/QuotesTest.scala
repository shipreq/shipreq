package shipreq.webapp.snippet

import shipreq.webapp.test.TestHelpers
import org.scalatest.FunSuite

class QuotesTest extends FunSuite with TestHelpers {

  test("All quotes should be single elem blockquotes") {
    for (q <- Quotes.quotes) {
      q.toString should (startWith("<blockquote") and endWith("</blockquote>"))
    }
  }
}
