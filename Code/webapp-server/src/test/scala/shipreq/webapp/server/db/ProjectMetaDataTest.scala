package shipreq.webapp.server.db

import shipreq.base.test.db.TestDb
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.{ActiveEvent, EventOrd, VerifiedEvent}
import shipreq.webapp.member.test.project.RandomEventStream
import shipreq.webapp.server.logic.util.Obfuscators
import shipreq.webapp.server.test.DbUtil
import shipreq.webapp.server.test.WebappServerTestUtil._
import utest._

/** Ensures that ProjectMetaData content always matches project content.
  */
object ProjectMetaDataTest extends TestSuite {

  override def tests = Tests {

    "test" - TestDb.withImperativeXA { xa =>
      val dbu = DbUtil(xa)
      import dbu.dbAlgebra

      val uid = dbu.newUserId()

      // Do this twice to ensure that other projects' events don't interfere
      for (_ <- 1 to 2) {
        val (_, ves1, ves2) = RandomEventStream.activeOnly.entireEventStream(50).samples().next()
        val initEvents = ves1.length

        val pid = dbu.newProjectId(uid, ves1.map(_.event.active))
        val pidPub = Obfuscators.projectId.obfuscate(pid)

        def writeEvent(ve: VerifiedEvent, idx: Int, p: Project): Unit =
          ve.event match {
            case ae: ActiveEvent =>
              val r = xa ! dbAlgebra.saveProjectEvent(pid, EventOrd.fromIndex(idx), ae, p, uid)
              r.getOrThrow()
              ()
            case x =>
              fail("Can't create non-active event: " + x)
          }

        // Mandatory events first
        var p = applyVerifiedEventSuccessfully(Project.empty, ves1: _*)

        for (idx <- ves2.indices) {
          val ve = ves2(idx)
          val idx2 = idx + initEvents
          p = applyEventSuccessfully(p, ve.event)
          writeEvent(ve, idx2, p)

          val md = (xa ! dbAlgebra.getAllProjectMetaDataForUser(uid)).find(_.id == pidPub).getOrElse(
            fail(s"ProjectMetaData not found for $pid."))

          val expectTotal = idx2 + 1
          val expectNonInit = expectTotal - initEvents
          assertEq("totalEventCount", md.eventsTotal, expectTotal)
          assertEq("nonInitEventCount", md.eventsPostInit, expectNonInit)
          md.assertInSyncWith(p)
        }
      }
    }

  }
}
