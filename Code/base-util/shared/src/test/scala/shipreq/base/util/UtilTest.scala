package shipreq.base.util

import utest._
import japgolly.microlibs.testutil.TestUtil._
import japgolly.univeq.UnivEqScalaz._

object UtilTest extends TestSuite {

  override def tests = Tests {
    'partitionConsecutive {
      def test(in: Int*)(a: Int*)(b: Int*) =
        assertEq(Util.partitionConsecutive(in.toList), (a.toList, b.toList))
      * - test()()()
      * - test(3)(3)()
      * - test(3, 4)(3, 4)()
      * - test(3, 5)(3)(5)
      * - test(3, 5, 6)(3)(5, 6)
      * - test(3, 4, 6)(3, 4)(6)
      * - test(3, 4, 5, 6)(3, 4, 5, 6)()
    }
  }
}
