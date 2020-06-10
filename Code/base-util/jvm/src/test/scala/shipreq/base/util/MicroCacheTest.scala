package shipreq.base.util

import japgolly.microlibs.testutil.TestUtil._
import java.time._
import scalaz.std.anyVal._
import utest._

object MicroCacheTest extends TestSuite {

  def fn: () => Int = {
    var i = 0
    () => {i += 1; i}
  }

  override def tests = Tests {

    "CacheVar" - {
      def newCacheVar(policy: CachePolicy[Any]) = {
        val f = fn
        val c = CacheVar[Int](policy)
        new {def getOrSet(): Any = c.getOrSet(f())}
      }

      "NeverExpire policy: should always return the same value" - {
        val c = newCacheVar(NeverExpire)
        assert(c.getOrSet() == 1)
        assert(c.getOrSet() == 1)
        assert(c.getOrSet() == 1)
      }

      "DisableCache policy: should always return a new value" - {
        val c = newCacheVar(DisableCache)
        assert(c.getOrSet() == 1)
        assert(c.getOrSet() == 2)
        assert(c.getOrSet() == 3)
      }
    }

    "CacheFn" - {
      def newCacheFn(policy: CachePolicy[Any]) = { val f = fn; CacheFn[Int](f())(policy) }

      "NeverExpire policy: should always return the same value" - {
        val c = newCacheFn(NeverExpire)
        assertEq(c.value, 1)
        assertEq(c.value, 1)
        assertEq(c.value, 1)
      }
      "DisableCache policy: should always return a new value" - {
        val c = newCacheFn(DisableCache)
        assertEq(c.value, 1)
        assertEq(c.value, 2)
        assertEq(c.value, 3)
      }
    }

    "ExpireAfter Policy" - {
      "should expire stuff after the given time limit" - {
        val ttl = Duration.ofMinutes(5)
        var now = Instant.now()
        val p = ExpireAfter(ttl, () => now)
        val t = p.write(())
        assertEq(p.expired(t), false)
        now = now plusSeconds 299
        assertEq(p.expired(t), false)
        now = now plusSeconds 2
        assertEq(p.expired(t), true)
      }
    }

  }
}
