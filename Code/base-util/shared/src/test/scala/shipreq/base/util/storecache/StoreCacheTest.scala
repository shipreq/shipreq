package shipreq.base.util.storecache.fake

import shipreq.base.util.storecache.StoreCache
import japgolly.microlibs.testutil.TestUtil._
import utest._

object StoreCacheTest extends TestSuite {

  private case class X(str: String, int: Int)

  private def testHorizontalComposition(strict: Boolean): Unit = {
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

    val init: (=> X) => l.Cache = strict match {
      case true  => l.initStrict(_)
      case false => l.initLazy(_)
    }

    val next: (l.Cache, => X) => l.Cache = strict match {
      case true  => l.nextStrict(_, _)
      case false => l.nextLazy(_, _)
    }

    // Shouldn't run until required
    val a = init(X("x", 4))
    assertEq(counts(), (0, 0, 0))

    // First get, run on demand
    assertEq(a.value, "x|x:8")
    assertEq(counts(), (1, 1, 1))

    // Second get doesn't rerun
    assertEq(a.value, "x|x:8")
    assertEq(counts(), (1, 1, 1))

    // New inputs same as old, shouldn't rerun
    val b = next(a, X("x", 4))
    assertEq(b.value, "x|x:8")
    assertEq(counts(), (1, 1, 1))

    // One new input
    val c = next(b, X("x", 5))
    assertEq(c.value, "x|x:10")
    assertEq(counts(), (1, 2, 2))

    // Same inputs
    val d = next(c, X("x", 5))
    assertEq(d.value, "x|x:10")
    assertEq(counts(), (1, 2, 2))

    // One new input
    val e = next(d, X("y", 5))
    assertEq(e.value, "y|y:10")
    assertEq(counts(), (2, 2, 3))
  }

  override def tests = Tests {

    "strict" - {
      "horizontal" - testHorizontalComposition(strict = true)
    }

    "lazy" - {

      "horizontal" - testHorizontalComposition(strict = false)

      "input" - {

        var countS = 0
        var nextStr = "x"
        def str() = {
          countS += 1
          nextStr
        }

        var countM = 0
        def modS(s: String): String = {
          countM += 1
          s"$s|$s"
        }

        def counts() = (countS, countM)

        val l = StoreCache.Logic(modS)

        // Shouldn't eval input until required
        val a = l.initLazy(str())
        assertEq(counts(), (0, 0))

        // First get, eval input
        assertEq(a.value, "x|x")
        assertEq(counts(), (1, 1))

        // Second get doesn't eval input
        assertEq(a.value, "x|x")
        assertEq(counts(), (1, 1))

        // New inputs same as old, shouldn't eval input until .value called
        val b = l.nextLazy(a, str())
        assertEq(counts(), (1, 1))
        assertEq(b.value, "x|x")
        assertEq(counts(), (2, 1))
        assertEq(b.value, "x|x")
        assertEq(counts(), (2, 1))

        // Different input
        nextStr = "y"
        val c = l.nextLazy(b, str())
        assertEq(counts(), (2, 1))
        assertEq(c.value, "y|y")
        assertEq(counts(), (3, 2))
        assertEq(c.value, "y|y")
        assertEq(counts(), (3, 2))
      }
    }

  }
}
