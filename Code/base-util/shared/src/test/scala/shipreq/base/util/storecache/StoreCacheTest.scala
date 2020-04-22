package shipreq.base.util.storecache.fake

import shipreq.base.util.storecache.StoreCache
import japgolly.microlibs.testutil.TestUtil._
import utest._

object StoreCacheTest extends TestSuite {

  private case class X(str: String, int: Int)

  override def tests = Tests {
    "twoComposed" - {

      var countS = 0
      def modS(s: String): String = {
        countS += 1
        s"$s|$s"
      }

      var countI = 0
      def modI(i: Int): Int = {
        countI += 1
        i * 2
      }

      var countC = 0
      def combine(s: String, i: Int): String = {
        countC += 1
        s"$s:$i"
      }

      def counts() = (countS, countI, countC)

      val ls = StoreCache.Logic(modS).contramap[X](_.str)
      val li = StoreCache.Logic(modI).contramap[X](_.int)
      val l  = StoreCache.Logic.apply2(ls, li)(combine)

      // Shouldn't run until required
      val a = l.init(X("x", 4))
      assertEq(counts(), (0, 0, 0))

      // First get, run on demand
      assertEq(a.value, "x|x:8")
      assertEq(counts(), (1, 1, 1))

      // Second get doesn't rerun
      assertEq(a.value, "x|x:8")
      assertEq(counts(), (1, 1, 1))

      // New inputs same as old, shouldn't rerun
      val b = l.next(a, X("x", 4))
      assertEq(b.value, "x|x:8")
      assertEq(counts(), (1, 1, 1))

      // One new input
      val c = l.next(b, X("x", 5))
      assertEq(c.value, "x|x:10")
      assertEq(counts(), (1, 2, 2))

      // Same inputs
      val d = l.next(c, X("x", 5))
      assertEq(d.value, "x|x:10")
      assertEq(counts(), (1, 2, 2))

      // One new input
      val e = l.next(d, X("y", 5))
      assertEq(e.value, "y|y:10")
      assertEq(counts(), (2, 2, 3))
    }
  }
}
