package shipreq.webapp.snippet

import shipreq.webapp.test.TestHelpers
import org.scalatest.FunSpec

class LinkSnippetTest extends FunSpec with TestHelpers {

  describe("Linking to a page") {
    it("should render a link to the expected page") {
      Link.ToPage("register1")(<a></a>) shouldBe <a href="/register">Register</a>
    }

    it("should override the href") {
      Link.ToPage("register1")(<a href="xxx"></a>) shouldBe <a href="/register">Register</a>
    }

    it("should preserve custom titles") {
      Link.ToPage("login")(<a data-lift="asdf">YAY!</a>) shouldBe <a href="/login">YAY!</a>
    }

    it("should preserve custom attributes") {
      Link.ToPage("login")(<a class="yes" data-lift="asdf"></a>) shouldBe <a class="yes" href="/login">Login</a>
    }

    it("should throw an exception if page not found") {
      intercept[Exception](Link.ToPage("xcbv"))
    }
  }

  it("should render a jquery link") {
    Link.dispatch("jquery")(<div></div>).toString should( include("jquery") and include(".js") and include("<script"))
  }
}
