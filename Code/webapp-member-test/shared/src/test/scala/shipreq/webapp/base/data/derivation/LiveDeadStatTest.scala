package shipreq.webapp.base.data.derivation

import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.fp.Monoid.Implicits._
import shipreq.webapp.base.data._
import utest._

object LiveDeadStatTest extends TestSuite {

  override def tests = Tests {

    "builder" - {
      "ofInts" - {
        val b = LiveDeadStat.Builder.ofInts()
        b.add(Live, 3)
        b.add(Dead, 99)
        b.add(Live, 1)
        b.add(Dead, 2)
        assertEq(b.result(), LiveDeadStat(4, 101))
      }
      "vec" - {
        val b = LiveDeadStat.Builder.vec[Int]()
        b.add(Live, 3)
        b.add(Dead, 99)
        b.add(Live, 1)
        b.add(Dead, 2)
        assertEq(b.result(), LiveDeadStat(Vector(3, 1), Vector(99, 2)))
      }
    }

    "append" - {
      val a = LiveDeadStat(3, 200)
      val b = LiveDeadStat(5, 1000)
      assertEq(a + b, LiveDeadStat(8, 1200))
    }

  }
}
