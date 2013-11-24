package com.beardedlogic.usecase.feature

import org.scalatest.{Matchers, FunSpec}
import com.beardedlogic.usecase.lib.Types._
import UcFilter._
import UcFilters._

class UcFilterTest extends FunSpec with Matchers {

  implicit def autoId(i: Long): UseCaseIdentId = i.toLong.tag[IsUseCaseIdentId]

  def aLawfulFilter(f: UcFilter) {
    it("fromJson . toJson = id") {
      val j = toJson(f)
      val f2 = fromJson(j)
      f2 shouldBe f
    }
  }

  describe("Filter: All") {
    it should behave like aLawfulFilter(All)
  }

  describe("Filter: Whitelist(a,b,c)") {
    it should behave like aLawfulFilter(Whitelist(List(1, 3, Long.MaxValue)))
  }
  describe("Filter: Whitelist(Ø)") {
    it should behave like aLawfulFilter(Whitelist(List.empty))
  }
}
