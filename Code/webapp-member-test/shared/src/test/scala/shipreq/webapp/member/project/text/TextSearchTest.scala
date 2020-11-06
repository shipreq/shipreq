package shipreq.webapp.member.project.text

import shipreq.webapp.member.project.text.TextSearch._
import shipreq.webapp.member.test.WebappTestUtil._
import utest._

object TextSearchTest extends TestSuite {

  override def tests = Tests {
    "normalisation" - {
      assertEq(String valueOf Normaliser.ignoreCaseSingleSpaces("AB cd 3 EF").data, "ab cd 3 ef")
    }
  }
}
