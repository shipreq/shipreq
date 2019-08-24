package shipreq.webapp.base.protocol

import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.test.BaseTestUtil._
import utest._

object VersionTest extends TestSuite {

  private val v10 = Version.fromInts(1, 0)
  private val v11 = Version.fromInts(1, 1)
  private val v12 = Version.fromInts(1, 2)
  private val v20 = Version.fromInts(2, 0)
  private val v21 = Version.fromInts(2, 1)
  private val v22 = Version.fromInts(2, 2)

  override def tests = Tests {
    'comparison {
      val all = List(v10, v11, v12, v20, v21, v22)
      val flat = all.mapToOrder
      for {
        l <- all
        r <- all
        x = flat(l)
        y = flat(r)
        actual = Version.ordering.compare(l, r).signum
        expect = Ordering.Int.compare(x, y).signum
      } assertEq(s"$l cmp $r", actual, expect)
    }
  }
}
