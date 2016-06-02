package shipreq.webapp.server.snippet

import utest._
import shipreq.webapp.base.data.{Project, ProjectCatalogue, StaticField}
import shipreq.webapp.base.event.FieldStaticRemove
import shipreq.webapp.server.data.ProjectId
import shipreq.webapp.server.test.WebappServerTestUtil._
import shipreq.webapp.server.test._

object HomeSpaTest extends TestSuite {

  override def tests = TestSuite {

    'createProject {
      def test(name: String): Unit =
        UserFixture.Transaction { uf =>
          val dbu = uf.toDbUtil
          import dbu.dao
          val uid = uf.user1.id

          // Confirm starting empty
          assertEq(dao.getProjectCatalogue(uid), ProjectCatalogue(Nil))

          // Create
          val pi = HomeSpa.createProject(dao, uid, name)

          val pid = ProjectId.Extern.parseO(pi.id.value).get
          def events() = dao.findAllEvents(pid)
          def loadProject() = applyVerifiedEventSuccessfully(Project.empty, events().map(_._2): _*)

          // Immediate result
          assertEq("Immediate name", pi.name, name)
          assertEq("Immediate eventCount", pi.eventCount, 0)
          assertEq("Immediate reqCount", pi.reqCount, 0)

          // Reloaded result
          val pc = dao.getProjectCatalogue(uid)
          assertEq(pc.items.length, 1)
          val a = pc.items.head
          assertFields(pi, a)
            .assertEq(_.id)
            .assertEq("Reloaded name", _.name)
            .assertEq("Reloaded eventCount", _.eventCount)
            .assertEq("Reloaded reqCount", _.reqCount)
          assertEq("Event count",  events().length, 2)

          // Next event
          val nextSeq = events().maxBy(_._1.value)._1.succ
          val p = loadProject()
          val e = FieldStaticRemove(StaticField.StepGraph)
          val ve = verifyEvent(p, e)
          dao.createEvent(pid, nextSeq, e, ve.hashRecs)
          val a2 = dao.getProjectCatalogue(uid).items.head
          assertEq("Next eventCount", a2.eventCount, a.eventCount + 1)
          loadProject()
        }

      test("Yeah")
      test("Untitled")
    }

  }
}
