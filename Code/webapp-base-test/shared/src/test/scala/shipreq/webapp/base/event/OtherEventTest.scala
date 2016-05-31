package shipreq.webapp.base.event

import utest._
import shipreq.webapp.base.test.WebappTestUtil._
import ApplyEventTestFns._
import NoInitialEvents._

object OtherEventTest extends TestSuite {

  override def tests = TestSuite {

    'ProjectNameSet {
      'updates {
        val p  = _assertPass(ProjectNameSet("öljy loppui"))
        assertEq(p.name, "öljy loppui")
      }
      'rejects {
        assertFail("blank")(ProjectNameSet(""))
        assertFail("preprocess")(ProjectNameSet("   as   "))
      }
    }

  }
}
