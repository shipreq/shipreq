package shipreq.webapp.base.text

import utest._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.text.TextSearch._

object TextSearchTest extends TestSuite {

  override def tests = Tests {
    "normalisation" - {
      assertEq(String valueOf Normaliser.ignoreCaseSingleSpaces("AB cd 3 EF").data, "ab cd 3 ef")
    }
  }
}
