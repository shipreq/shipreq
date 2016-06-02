package shipreq.webapp.server.lib

import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.server.lib.Misc._

object MiscTest extends TestSuite {

  override def tests = TestSuite {
    'randomConfirmationToken {
      assert(randomConfirmationToken !=* randomConfirmationToken)
      assert(randomConfirmationToken !=* randomConfirmationToken)
      assert(randomConfirmationToken !=* randomConfirmationToken)
    }
  }
}
