package com.beardedlogic.usecase.snippet

import com.beardedlogic.usecase.test.TestHelpers
import org.scalatest.FunSpec

class LinkSnippetTest extends FunSpec with TestHelpers {

  describe("#to") {

    it("should render a link to the named page") {
      withSessionAttrs("name" -> "Register1") {
        Link.to(<div></div>).toString ==== """<a href="/register">Register</a>"""
      }
    }

    it("should throw an exception if page not found") {
      withSessionAttrs("name" -> "xcbv") {
        intercept[Exception](Link.to(<div></div>))
      }
    }
  }
}
