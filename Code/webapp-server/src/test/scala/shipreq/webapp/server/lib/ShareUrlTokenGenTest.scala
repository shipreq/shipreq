package shipreq.webapp.server.lib

import utest._
import shipreq.base.test.BaseTestUtil._

object ShareUrlTokenGenTest extends TestSuite {

  def next = ShareUrlTokenGen.fn()

  override def tests = Tests {

    'size - assertEq(next.value.length, ShareUrlTokenGen.len)

    'random {
      val ts = (1 to 100).toList.map(_ => next).distinct
      assert(ts.size > 95)
    }
  }
}
