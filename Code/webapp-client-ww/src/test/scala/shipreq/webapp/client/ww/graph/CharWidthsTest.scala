package shipreq.webapp.client.ww.graph

import japgolly.microlibs.testutil.TestUtil._
import utest._
import utest.framework.TestPath

object CharWidthsTest extends TestSuite {

  override def tests = Tests {
    "apply" - {
      def shouldBe(expect: Double)(implicit tp: TestPath): Unit = {
        val c = tp.value.last.toInt.toChar
        assertEq(CharWidths(c), expect)
      }
      "0" - shouldBe(0)
      "31" - shouldBe(0)
      "32" - shouldBe(2.703125)
      "33" - shouldBe(4.796875)
      "126" - shouldBe(8.125)
      "127" - shouldBe(0)
      "128" - shouldBe(5.144775)
      "129" - shouldBe(5.144775)
      "255" - shouldBe(5.144775)
      "256" - shouldBe(4.886353)
      "257" - shouldBe(4.886353)
      "512" - shouldBe(4.886353)
      "1023" - shouldBe(4.886353)
      "1024" - shouldBe(3.515182)
      "1025" - shouldBe(3.515182)
      "2047" - shouldBe(3.515182)
      "2048" - shouldBe(4.906052)
      "2049" - shouldBe(4.906052)
      "64511" - shouldBe(8.434891)
      "64512" - shouldBe(4.770737)
      "64513" - shouldBe(4.770737)
      "65535" - shouldBe(4.770737)
    }
  }
}
