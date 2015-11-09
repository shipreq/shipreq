package shipreq.base.util

import utest._
import ScalaExt._

object ScalaExtTest extends TestSuite {
  override def tests = TestSuite {
    'vectorInsert {
      for {
        vs <- List(Vector(), Vector(1), Vector(1, 2, 3))
        i  <- -2  to vs.length + 2
      } {
        val r = vs.insert(i, 666)
        if (i >= 0 && i <= vs.length) {
          assert(r.isDefined)
          val n = r.get
          assert(n(i) == 666)
          assert(n.filterNot(_ == 666) == vs)
        } else
          assert(r.isEmpty)
      }
    }
  }
}
