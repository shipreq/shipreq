package shipreq.base.util

import org.specs2.mutable.Specification
import org.specs2.matcher.ThrownExpectations

class BiMapTest extends Specification with ThrownExpectations {

  "Adding & retrieving" in {
    val b = new BiMapBuilder[String,Int]
    b += ("Three" -> 3)
    b("Two") = 2
    val m = b.result
    m.ba(3) ==== "Three"
    m.ab("Two") ==== 2
  }
}
