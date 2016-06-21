package shipreq.webapp.server.db

import utest._
import shipreq.webapp.base.data.{Project, ProjectCatalogueProps}
import shipreq.webapp.base.event.{ActiveEvent, RandomEventStream}
import shipreq.webapp.server.db.EventDao.EventSeq
import shipreq.webapp.server.test.TestDb
import shipreq.webapp.server.test.WebappServerTestUtil._

/** Ensures that ProjectCatalogue.Item content always matches project content.
  */
object ProjectCatalogueTest extends TestSuite {

  override def tests = TestSuite {

    TestDb.DbUtil { dbu =>
      val uid = dbu.newUserId()

      // Do this twice to ensure that other projects' events don't interfere
      for (_ <- 1 to 2) {
        val pid = dbu.newProjectId(uid)

        val (_, ves1, ves2) = RandomEventStream.entireEventStream(50).samples().next()
        val ves = ves1 ++ ves2
        var p = Project.empty
        for (idx <- ves.indices) {
          val ve = ves(idx)

          ve.event match {
            case ae: ActiveEvent =>
              dbu.dao.createEvent(pid, EventSeq(idx), ae, ve.hashRecs)
            case x =>
              fail("Can't create non-active event: " + x)
          }

          p = applyEventSuccessfully(p, ve.event)

          val i = dbu.dao.findProjectCatalogueItem(uid, pid) getOrElse
            fail(s"ProjectCatalogueItem not found for ($uid,$pid).")

          val e = (idx + 1 - RandomEventStream.InitialEventCount) max 0
          ProjectCatalogueProps(i, p, e).assert()
        }
      }
    }

  }
}
