package shipreq.webapp.member.project.event

import japgolly.microlibs.testutil.TestUtil._
import shipreq.webapp.member.test.WebappTestUtil.equalVerifiedEvent
import shipreq.webapp.member.test.project.RandomData
import utest._

object ProjectEventsTest extends TestSuite {

  override def tests = Tests {

    "needByOrd" - {
      val events = RandomData.events.verifiedEventSeq(100).sample()
      val pe = ProjectEvents(events)
      for (e <- events)
        assertEq(e, pe.need(e.ord))
    }

  }
}
