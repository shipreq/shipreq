package shipreq.webapp.server.logic.util

import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data.ProjectId
import utest._

/** Ensures that obfuscation algos don't change - important because they are being used in clients' urls */
object ObfuscatorsTest extends TestSuite {

  override def tests = Tests {
    "projectId" - {
      def test(i: Int, s: String): Unit =
        assertEq(Obfuscators.projectId.obfuscate(ProjectId(i)).value, s)
      "1" - test(1, "cUZ0")
      "2" - test(2, "t3f1")
      "7" - test(7, "o3qCX")
      "11" - test(11, "dGR7")
      "1024" - test(1024, "4cqXf")
      "5000" - test(5000, "ccQJY")
    }
  }
}
