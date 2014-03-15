package shipreq.base.util

import org.scalatest.FunSuite
import org.scalatest.Matchers

class BiMapTest extends FunSuite with Matchers {

  test("Adding & retrieving") {
    val b = new BiMapBuilder[String,Int]
    b += ("Three" -> 3)
    b("Two") = 2
    val m = b.result
    m.ba(3) shouldBe "Three"
    m.ab("Two") shouldBe 2
  }
}
