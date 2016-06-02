package shipreq.webapp.server.snippet

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class PageLinkTest extends FunSpec {

  describe("Linking to a page") {
    it("should render a link to the expected page") {
      PageLink.toPage("register1")(<a></a>) shouldBe <a href="/register">Register</a>
    }

    it("should override the href") {
      PageLink.toPage("register1")(<a href="xxx"></a>) shouldBe <a href="/register">Register</a>
    }

    it("should preserve custom titles") {
      PageLink.toPage("login")(<a data-lift="asdf">YAY!</a>) shouldBe <a href="/login">YAY!</a>
    }

    it("should preserve custom attributes") {
      PageLink.toPage("login")(<a class="yes" data-lift="asdf"></a>) shouldBe <a class="yes" href="/login">Login</a>
    }

    it("should throw an exception if page not found") {
      intercept[Exception](PageLink.toPage("xcbv"))
    }
  }
}
