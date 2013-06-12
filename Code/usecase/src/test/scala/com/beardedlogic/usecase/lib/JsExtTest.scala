package com.beardedlogic.usecase.lib

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import JsExt._

class JsExtTest extends FunSuite with ShouldMatchers {

  test("JqFadeIn with duration: default") {
    JqFadeIn().toJsCmd should equal("fadeIn()")
  }

  test("JqFadeIn with duration: fast") {
    JqFadeIn(Fast).toJsCmd should equal("fadeIn('fast')")
  }

  test("JqFadeIn with duration: milliseconds") {
    JqFadeIn(210.ms).toJsCmd should equal("fadeIn(210)")
  }
}
