package shipreq.base.util

import japgolly.microlibs.testutil.TestUtil._
import java.time.Duration
import utest._
import JavaTimeHelpers._

object JavaTimeHelpersTest extends TestSuite {

  override def tests = Tests {

    'toSeconds {
      val d = Duration.ofMillis(1100) plus Duration.ofNanos(9002003L)
      d.asSeconds ==> 1.109002003
    }

    'conciseDesc {
      def test(sec: Double, ns: Long, expect: String): Unit = {
        val d = Duration.ofSeconds(sec.toLong).plus(Duration.ofNanos(ns))
        val a = d.conciseDesc
        a ==> expect
      }

      'ns1 - test(0,   1,   "1 ns")
      'ns2 - test(0,  11,  "11 ns")
      'ns3 - test(0, 111, "111 ns")

      'us1 - test(0,   1001,   "1 us")
      'us2 - test(0,  11001,  "11 us")
      'us3 - test(0, 111001, "111 us")

      'ms1 - test(0,   1001001,   "1 ms")
      'ms2 - test(0,  11001001,  "11 ms")
      'ms3 - test(0, 111001001, "111 ms")

      'sec1 - test( 0, 1001001001,  "1.0 sec")
      'sec2 - test( 0, 1901001001,  "1.9 sec")
      'sec3 - test(59,  901001001, "59.9 sec")

      'min1 - test( 1.09 * 60, 3,  "1.1 min")
      'min2 - test(12.94 * 60, 3, "12.9 min")
      'min3 - test(59.88 * 60, 3, "59.9 min")

      'hr1 - test(1.02 * 3600, 3, "1.02 hr")
      'hr2 - test(23 * 3600, 3, "23.00 hr")

      'day1 - test(36 * 3600, 3, "1.50 days")
      'day2 - test(48 * 3600, 3, "2.00 days")
    }
  }
}
