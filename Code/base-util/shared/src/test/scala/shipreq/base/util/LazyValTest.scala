package shipreq.base.util

import japgolly.microlibs.testutil.TestUtil._
import utest._

object LazyValTest extends TestSuite {

  override def tests = Tests {
    "stackSafety" - {
      val n = 200000

      "map" - {
        val x = (1 to n).iterator.foldLeft(LazyVal(0))((q, _) => q.map(_ + 1))
        assertEq(x.value, n)
      }

      "flatMap" - {
        val x = (1 to n).iterator.foldLeft(LazyVal(0))((q, _) => q.flatMap(n => LazyVal(n + 1)))
        assertEq(x.value, n)
      }
    }

  }
}
