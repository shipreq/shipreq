package shipreq.webapp.base.data

import java.time.Instant
import utest._
import shipreq.webapp.base.event.RandomEventStream
import shipreq.webapp.base.test.WebappTestUtil._

object ProjectMetaDataTest extends TestSuite {

  override def tests = Tests {

    "applyEvent" - {
      val (_, vesInit, ves) = RandomEventStream.entireEventStream(100).samples().next()
      var p = applyVerifiedEventSuccessfully(Project.empty, vesInit: _*)
      var md = looseProjectMetaData(p, eventsTotal = vesInit.length)
      val now = Instant.now()
      for (ve <- ves) {
        p = applyEventSuccessfully(p, ve.event)
        md = md.applyEvent(ve, p, now)
        md.assertInSyncWith(p)
      }
    }

  }
}
