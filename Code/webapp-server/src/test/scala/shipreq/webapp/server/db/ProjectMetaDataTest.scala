package shipreq.webapp.server.db

import utest._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{ActiveEvent, EventOrd, RandomEventStream, VerifiedEvent}
import shipreq.webapp.server.test.{DbUtil, PrepareEnv}
import shipreq.webapp.server.test.WebappServerTestUtil._

/** Ensures that ProjectMetaData content always matches project content.
  */
object ProjectMetaDataTest extends TestSuite {
  import PrepareEnv.dbAlgebra

  override def tests = Tests {

    DbUtil.use().runNow { dbu =>
      import dbu.xa

      val uid = dbu.newUserId()

      // Do this twice to ensure that other projects' events don't interfere
      for (_ <- 1 to 2) {
        val pid = dbu.newProjectId(uid)

        def writeEvent(ve: VerifiedEvent, idx: Int): Unit =
          ve.event match {
            case ae: ActiveEvent =>
              xa ! dbAlgebra.saveProjectEvent(pid)(EventOrd(idx), ae, ve.hashRecs)
            case x =>
              fail("Can't create non-active event: " + x)
          }

        val (_, ves1, ves2) = RandomEventStream.entireEventStream(50).samples().next()

        // Mandatory events first
        ves1.zipWithIndex.foreach((writeEvent _).tupled)
        var p = applyVerifiedEventSuccessfully(Project.empty, ves1: _*)

        for (idx <- ves2.indices) {
          val ve = ves2(idx)
          val seq = idx + ves1.length
          writeEvent(ve, seq)
          p = applyEventSuccessfully(p, ve.event)

          val md = xa ! dbAlgebra.getProjectMetaData(pid) getOrElse
            fail(s"ProjectMetaData not found for $pid.")

          val e = (seq + 1 - RandomEventStream.InitialEventCount) max 0
          assert(md.eventCount ==* e)
          md.assertInSyncWith(p)
        }
      }
    }

  }
}
