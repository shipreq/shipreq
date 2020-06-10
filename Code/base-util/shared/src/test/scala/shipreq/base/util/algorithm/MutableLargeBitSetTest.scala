package shipreq.base.util.algorithm

import japgolly.microlibs.testutil.TestUtil._
import utest._

object MutableLargeBitSetTest extends TestSuite {

  val bitsLens = List(1, 2, 3, 4, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 66, 127, 128, 129)

  override def tests = Tests {
    "flipBit" - {
      for (bits <- bitsLens) {
        val bs = MutableLargeBitSet(bits).clear()
        for (b <- 0 until bits) {
          bs.flipBit(b)
          for (i <- 0 until bits) {
            val a = bs.bit(i)
            val e = i == b
            assertEq(s"MutableLargeBitSet($bits).flipBit($b).bit($i)", a, e)
          }
          bs.flipBit(b)
        }

      }
    }
  }
}
