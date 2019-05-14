package shipreq.webapp.server.snippet

import java.time.Instant
import utest._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.FieldStaticRemove
import shipreq.webapp.server.logic.DB.SaveProjectEventCmd
import shipreq.webapp.server.logic.{HomeSpaLogic, Obfuscators}
import shipreq.webapp.server.test.WebappServerTestUtil._
import shipreq.webapp.server.test._

object HomeSpaTest extends TestSuite {
  implicit def db = PrepareEnv.dbAlgebra

  override def tests = Tests {

    'createProject {
      def test(name: String): Unit =
        UserFixture.Transaction.runNow { uf =>
          import uf.xa
          val uid = uf.user1.id

          // Confirm starting empty
          assertEq(xa ! db.getAllProjectMetaDataForUser(uid), Nil)

          // Create
          val pi = xa ! HomeSpaLogic.createProject(uid, name, Instant.now())
          val initEvents = 2

          val pid = Obfuscators.projectId.deobfuscate(pi.id).toOption.get
          def events() = (xa ! db.getAllProjectEvents(pid)).toVector
          def loadProject() = applyVerifiedEventSuccessfully(Project.empty, events(): _*)

          // Immediate result
          assertEq("Immediate name", pi.name, name)
          assertEq("Immediate.initEventCount", pi.initEventCount, initEvents)
          assertEq("Immediate.totalEventCount", pi.totalEventCount, initEvents)
          assertEq("Immediate.nonInitEventCount", pi.nonInitEventCount, 0)
          assertEq("Immediate reqCount", pi.reqCount, 0)

          // Reloaded result
          val pc = xa ! db.getAllProjectMetaDataForUser(uid)
          assertEq(pc.length, 1)
          val a = pc.head
          assertFields(pi, a)
            .assertEq(_.id)
            .assertEq("Reloaded name", _.name)
            .assertEq("Reloaded.nonInitEventCount", _.nonInitEventCount)
            .assertEq("Reloaded reqCount", _.reqCount)
          assertEq("Event count", events().length, 2)

          // Next event
          val nextOrd = events().maxBy(_.ord.value).ord + 1
          val p = loadProject()
          val e = FieldStaticRemove(StaticField.StepGraph)
          val ve = verifyEvent(p, e)
          val cmd = SaveProjectEventCmd(nextOrd, e, ve.hashRecs)
          xa ! db.saveProjectEvent(pid, cmd)
          val a2 = (xa ! db.getAllProjectMetaDataForUser(uid)).head
          assertEq("Next.nonInitEventCount", a2.nonInitEventCount, a.nonInitEventCount + 1)
          loadProject()
        }

      test("Yeah")
      test("Untitled")
    }

  }
}
