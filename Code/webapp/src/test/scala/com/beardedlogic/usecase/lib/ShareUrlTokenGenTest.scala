package com.beardedlogic.usecase.lib

import org.scalatest.{Matchers, FunSuite}

class ShareUrlTokenGenTest extends FunSuite with Matchers {
  def next = ShareUrlTokenGen.fn()

  test("Size") {
    next.size shouldBe ShareUrlTokenGen.len
  }
  test("Random") {
    val ts = (1 to 100).toList.map(_ => next).distinct
    ts.size should be > 95
  }
}
