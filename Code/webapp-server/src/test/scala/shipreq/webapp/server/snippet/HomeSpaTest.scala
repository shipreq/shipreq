package shipreq.webapp.server.snippet

import java.time.Instant
import utest._
import shipreq.webapp.base.data.{Project, StaticField}
import shipreq.webapp.base.event.FieldStaticRemove
import shipreq.webapp.server.app.Interpreters
import shipreq.webapp.server.db.DbLogic
import shipreq.webapp.server.logic.{HomeSpaLogic, ProjectId}
import shipreq.webapp.server.test.WebappServerTestUtil._
import shipreq.webapp.server.test._

object HomeSpaTest extends TestSuite {
  import Interpreters.dbAlgebra

  override def tests = TestSuite {

    'createProject {
      def test(name: String): Unit =
        UserFixture.Transaction.runNow { uf =>
          import uf.xa
          val uid = uf.user1.id

          // Confirm starting empty
          assertEq(xa ! DbLogic.project.findAllProjectMetaDataForUser(uid), Nil)

          // Create
          val pi = xa ! HomeSpaLogic.createProject(uid, name, Instant.now())

          val pid = ProjectId.Extern.parseOption(pi.id.value).get
          def events() = xa ! DbLogic.event.findAll(pid)
          def loadProject() = applyVerifiedEventSuccessfully(Project.empty, events().map(_._2): _*)

          // Immediate result
          assertEq("Immediate name", pi.name, name)
          assertEq("Immediate eventCount", pi.eventCount, 0)
          assertEq("Immediate reqCount", pi.reqCount, 0)

          // Reloaded result
          val pc = xa ! DbLogic.project.findAllProjectMetaDataForUser(uid)
          assertEq(pc.length, 1)
          val a = pc.head
          assertFields(pi, a)
            .assertEq(_.id)
            .assertEq("Reloaded name", _.name)
            .assertEq("Reloaded eventCount", _.eventCount)
            .assertEq("Reloaded reqCount", _.reqCount)
          assertEq("Event count", events().length, 2)

          // Next event
          val nextOrd = events().maxBy(_._1.value)._1 + 1
          val p = loadProject()
          val e = FieldStaticRemove(StaticField.StepGraph)
          val ve = verifyEvent(p, e)
          xa ! DbLogic.event.create(pid, nextOrd, e, ve.hashRecs)
          val a2 = (xa ! DbLogic.project.findAllProjectMetaDataForUser(uid)).head
          assertEq("Next eventCount", a2.eventCount, a.eventCount + 1)
          loadProject()
        }

      test("Yeah")
      test("Untitled")
    }

  }
}
