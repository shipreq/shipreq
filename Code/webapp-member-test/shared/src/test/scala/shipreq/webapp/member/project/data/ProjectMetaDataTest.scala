package shipreq.webapp.member.project.data

import java.time.Instant
import shipreq.webapp.base.data.UserId
import shipreq.webapp.base.util.Obfuscated
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.RandomEventStream
import utest._

object ProjectMetaDataTest extends TestSuite {

  override def tests = Tests {

    "applyEvent" - {
      val (_, vesInit, ves) = RandomEventStream.entireEventStream(100).samples().next()
      var p = applyVerifiedEventSuccessfully(emptyProject1, vesInit: _*)
      var md = looseProjectMetaData(p, eventsTotal = vesInit.length)
      val now = Instant.now()
      val uid: UserId.Public = Obfuscated("")
      for (ve <- ves) {
        p = applyEventSuccessfully(p, ve.event)
        md = md.applyEvent(uid, ve, p, now)
        md.assertInSyncWith(p)
      }
    }

  }
}
