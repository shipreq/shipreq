package shipreq.webapp.server.db

import utest._
import shipreq.webapp.base.data.{Project, ProjectCatalogueProps}
import shipreq.webapp.base.event.{ActiveEvent, RandomEventStream, VerifiedEvent}
import shipreq.webapp.server.test.{DbUtil, TestDb}
import shipreq.webapp.server.test.WebappServerTestUtil._

/** Ensures that ProjectCatalogue.Item content always matches project content.
  */
object ProjectCatalogueTest extends TestSuite {

  override def tests = TestSuite {

    DbUtil.use().runNow { dbu =>
      import dbu.xa

      val uid = dbu.newUserId()

      // Do this twice to ensure that other projects' events don't interfere
      for (_ <- 1 to 2) {
        val pid = dbu.newProjectId(uid)

        def writeEvent(ve: VerifiedEvent, idx: Int): Unit =
          ve.event match {
            case ae: ActiveEvent =>
              xa ! DbLogic.event.create(pid, EventSeq(idx), ae, ve.hashRecs)
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

          val i = xa ! DbLogic.project.findCatalogueItem(uid, pid) getOrElse
            fail(s"ProjectCatalogueItem not found for ($uid,$pid).")

          val e = (seq + 1 - RandomEventStream.InitialEventCount) max 0
          ProjectCatalogueProps(i, p, e).assert()
        }
      }
    }

  }
}
