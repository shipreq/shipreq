package shipreq.webapp.server.db

import utest._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{ActiveEvent, EventOrd, RandomEventStream, VerifiedEvent}
import shipreq.webapp.server.logic.{DB, Obfuscators}
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
      val initEvents = RandomEventStream.InitialEventCount
      val idxToOrd = initEvents + 1

      // Do this twice to ensure that other projects' events don't interfere
      for (_ <- 1 to 2) {
        val pid = dbu.newProjectId(uid, initEvents)
        val pidPub = Obfuscators.projectId.obfuscate(pid)

        def writeEvent(ve: VerifiedEvent, idx: Int): Unit =
          ve.event match {
            case ae: ActiveEvent =>
              val cmd = DB.SaveProjectEventCmd(EventOrd(idx + idxToOrd), ae)
              xa ! dbAlgebra.saveProjectEvent(pid, cmd)
            case x =>
              fail("Can't create non-active event: " + x)
          }

        val (_, ves1, ves2) = RandomEventStream.entireEventStream(50).samples().next()

        // Mandatory events first
        ves1.zipWithIndex.foreach((writeEvent _).tupled)
        var p = applyVerifiedEventSuccessfully(Project.empty, ves1: _*)

        for (idx <- ves2.indices) {
          val ve = ves2(idx)
          val idx2 = idx + ves1.length
          writeEvent(ve, idx2)
          p = applyEventSuccessfully(p, ve.event)

          val md = (xa ! dbAlgebra.getAllProjectMetaDataForUser(uid)).find(_.id == pidPub).getOrElse(
            fail(s"ProjectMetaData not found for $pid."))

          val expectTotal = idx2 + idxToOrd
          val expectNonInit = expectTotal - initEvents
          assert(md.totalEventCount ==* expectTotal)
          assert(md.nonInitEventCount ==* expectNonInit)
          md.assertInSyncWith(p)
        }
      }
    }

  }
}
