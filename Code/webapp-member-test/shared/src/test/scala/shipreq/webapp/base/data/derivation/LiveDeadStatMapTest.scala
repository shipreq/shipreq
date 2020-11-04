package shipreq.webapp.base.data.derivation

import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.LazyVal
import shipreq.base.util.fp.Monoid
import shipreq.base.util.fp.Monoid.Implicits._
import shipreq.webapp.base.data.{Dead, Live}
import utest._

object LiveDeadStatMapTest extends TestSuite {

  private def make[K, V: Monoid](kvs: (K, LiveDeadStat[V])*)(allLive: V, allDead: V): LiveDeadStatMap[K, V] = {
    val m = kvs.toMap
    val all = LazyVal(LiveDeadStat(allLive, allDead))
    LiveDeadStatMap(m, all)
  }

  override def tests = Tests {

    "builder" - {
      "ofInts" - {
        val b = LiveDeadStatMap.Builder.ofInts[Int]()
        b(99).add(Live, 3)
        b(50).add(Live, 3000)
        b(99).add(Live, 10)
        b(99).add(Dead, 500)
        val expect = make(99 -> LiveDeadStat(13, 500), 50 -> LiveDeadStat(3000, 0))(3013, 500)
        assertEq(b.result(), expect)
      }
      "vec" - {
        val b = LiveDeadStatMap.Builder.vec[Int, Int]()
        b(99).add(Live, 3)
        b(50).add(Live, 3000)
        b(99).add(Live, 10)
        b(99).add(Dead, 500)
        val expect = make(
          50 -> LiveDeadStat(Vector(3000), Vector()),
          99 -> LiveDeadStat(Vector(3, 10), Vector(500)),
        )(Vector(3000, 3, 10), Vector(500))
        assertEq(b.result(), expect)
      }
    }

    "append" - {
      val a = make(1 -> LiveDeadStat(10, 1000), 2 -> LiveDeadStat(22, 2222))(32, 3222)
      val e = make(1 -> LiveDeadStat(20, 2000), 2 -> LiveDeadStat(44, 4444))(64, 6444)
      assertEq(a ++ a, e)
    }
  }
}
