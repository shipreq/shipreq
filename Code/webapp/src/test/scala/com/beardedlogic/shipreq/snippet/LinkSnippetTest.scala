package com.beardedlogic.shipreq.snippet

import com.beardedlogic.shipreq.test.TestHelpers
import org.scalatest.FunSpec

class LinkSnippetTest extends FunSpec with TestHelpers {

  describe("#to") {

    it("should render a link to the named page") {
      withSessionAttrs("name" -> "register1") {
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
