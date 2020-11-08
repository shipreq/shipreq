package shipreq.webapp.server.snippet

import doobie.ConnectionIO
import shipreq.base.db.scalazDoobieConnectionIO
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.Event.FieldStaticRemove
import shipreq.webapp.server.logic.algebra.Crypto
import shipreq.webapp.server.logic.impl.HomeSpaLogic
import shipreq.webapp.server.logic.util.Obfuscators
import shipreq.webapp.server.test.WebappServerTestUtil._
import shipreq.webapp.server.test._
import utest._

object HomeSpaTest extends TestSuite {

  override def tests = Tests {

    "createProject" - {
      def test(name: String): Unit =
        UserFixture.use { uf =>
          import uf.xa
          val uid = uf.user1.id
          implicit val db = uf.dbUtil.dbAlgebra
          implicit val crypto = Crypto.default[ConnectionIO]

          // Confirm starting empty
          assertEq(xa ! db.getAllProjectMetaDataForUser(uid), Nil)

          // Create
          val pi = xa ! HomeSpaLogic.createProject[ConnectionIO](uid, name)
          val initEvents = 2

          val pid = Obfuscators.projectId.deobfuscate(pi.id).toOption.get
          def events() = (xa ! db.getAllProjectEvents(pid)).getOrThrow().toVector
          def loadProject() = applyVerifiedEventSuccessfully(Project.empty, events(): _*)

          // Immediate result
          assertEq("Immediate name", pi.name, name)
          assertEq("Immediate.eventsInit", pi.eventsInit, initEvents)
          assertEq("Immediate.eventsTotal", pi.eventsTotal, initEvents)
          assertEq("Immediate.eventsPostInit", pi.eventsPostInit, 0)
          assertEq("Immediate reqsLive", pi.reqsLive, 0)
          assertEq("Immediate reqsTotal", pi.reqsTotal, 0)

          // Reloaded result
          val pc = xa ! db.getAllProjectMetaDataForUser(uid)
          assertEq(pc.length, 1)
          val a = pc.head
          assertFields(pi, a)
            .assertEq(_.id)
            .assertEq("Reloaded name", _.name)
            .assertEq("Reloaded.eventsPostInit", _.eventsPostInit)
            .assertEq("Reloaded reqsLive", _.reqsLive)
            .assertEq("Reloaded reqsTotal", _.reqsTotal)
          assertEq("Event count", events().length, 2)

          // Next event
          val nextOrd = events().maxBy(_.ord.value).ord + 1
          val p = loadProject()
          val e = FieldStaticRemove(StaticField.StepGraph)
          val ve = verifyEvent(p, e)
          val p2 = applyVerifiedEventSuccessfully(p, ve)
          xa ! db.saveProjectEvent(pid, nextOrd, e, p2, uid)
          val a2 = (xa ! db.getAllProjectMetaDataForUser(uid)).head
          assertEq("Next.nonInitEventCount", a2.eventsPostInit, a.eventsPostInit + 1)
          loadProject()
        }

      test("Yeah")
      test("Untitled")
    }

  }
}
