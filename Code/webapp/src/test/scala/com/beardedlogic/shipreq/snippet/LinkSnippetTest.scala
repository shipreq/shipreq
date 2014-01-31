package com.beardedlogic.shipreq.snippet

import com.beardedlogic.shipreq.test.TestHelpers
import org.scalatest.FunSpec

class LinkSnippetTest extends FunSpec with TestHelpers {

  describe("Linking to a page") {
    it("should render a link to the named page") {
      Link.ToPage("register1")(<div></div>).toString ==== """<a href="/register">Register</a>"""
    }

    it("should preserve custom titles") {
      Link.ToPage("login")(<a data-lift="asdf">YAY!</a>).toString ==== """<a href="/login">YAY!</a>"""
    }

    it("should throw an exception if page not found") {
      intercept[Exception](Link.ToPage("xcbv"))
    }
  }

  it("should render a jquery link") {
    Link.dispatch("jquery")(<div></div>).toString should( include("jquery") and include(".js") and include("<script"))
  }
}
