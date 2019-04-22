package shipreq.webapp.server.logic

import scalaz.{-\/, Name}
import utest._
import shipreq.base.ops.Trace
import shipreq.base.util.Direction
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.protocol.CreateContentCmd
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.user._

object ProjectServerTest extends TestSuite {

  class Tester extends MockInterpreters {
    val logic = ProjectServer[Name, Name](ProjectServer.BroadcastTo.All)
  }

  val pid = ProjectId(100)
  val pid2 = ProjectId(101)
  val uid = UserId(200)
  val uid2 = UserId(201)
  val usr = Username("bob")
  val newUC = CreateContentCmd.CreateUseCase(Set.empty, Map.empty, Direction.Values.both(Set.empty), Set.empty, Vector.empty)

  val nop = (_: Any) => Name(())

  override def tests = Tests {

    'registrationAndLoading {
      val t = new Tester; import t._
      db.addProject(pid, uid)()
      def test(id: String, storeSize: Int, dbLoadHead: Int, dbLoadBody: Int): Unit =
        assertEq(id,
          (storeMap.size, db.loadProjectHeaderLog.length, db.loadProjectMetaDataLog.length, db.loadProjectLog.length),
          (storeSize, dbLoadHead, dbLoadBody, dbLoadBody))

      test("[A]", 0, 0, 0); val regId1 = logic.register(pid, uid, nop).value.needRight
      test("[B]", 1, 1, 0);              svr.runForked()
      test("[C]", 1, 1, 1); val regId2 = logic.register(pid, uid, nop).value.needRight; svr.runForked()
      test("[D]", 1, 1, 1);              logic.unregister(regId1).value
      test("[E]", 1, 1, 1); val regId3 = logic.register(pid, uid, nop).value.needRight; svr.runForked()
      test("[F]", 1, 1, 1);              logic.unregister(regId3).value
      test("[G]", 1, 1, 1);              logic.unregister(regId3).value // ignored
      test("[H]", 1, 1, 1);              logic.unregister(regId2).value
      test("[I]", 0, 1, 1); val regId4 = logic.register(pid, uid, nop).value.needRight
      test("[J]", 1, 2, 1);              svr.runForked()
      test("[K]", 1, 2, 2)
    }

    'registerNoProject {
      val t = new Tester; import t._
      assertEq(logic.register(pid, uid, nop).value, -\/(ProjectServer.ProjectNotFound))
    }

    'registerNotOwner {
      val t = new Tester; import t._
      db.addProject(pid, uid2)()
      def test() = assertEq(logic.register(pid, uid, nop).value, -\/(ProjectServer.AccessDenied))

      test()
      logic.register(pid, uid2, nop).value.needRight // Load with correct user so project is in store
      test()
    }

    // TODO This is all being replaced...

//    'updatesAndListeners {
//      val t = new Tester; import t._
//      db.addProject(pid, uid)()
//
//      var recv1 = Vector.empty[VerifiedEvent.NonEmptySeq]
//      val regId1 = logic.register(pid, uid, ve => Name(recv1 :+= ve)).value.needRight
//      val client1 = logic.initialClient(regId1, usr).value.needRight
//      val asyncData1 = svr.run(client1.initAsync)(()).needRight
//      assertEq("[1]", recv1, Vector.empty)
//
//      val ves1 = svr.run(client1.createContent)(newUC).needRight.needNES
//      assertEq("[2]", recv1, Vector(ves1)) // Because BroadcastTo.All
//
//      var recv2 = Vector.empty[VerifiedEvent.NonEmptySeq]
//      val regId2 = logic.register(pid, uid, ve => Name(recv2 :+= ve)).value.needRight
//      val client2 = logic.initialClient(regId2, usr).value.needRight
//      val asyncData2 = svr.run(client2.initAsync)(()).needRight
//      assertEq("[3]", recv2, Vector.empty)
//      assertEq("[4]", recv1, Vector(ves1))
//      assertEq("[5]", asyncData2.latestEventOrd, asyncData1.latestEventOrd + 1)
//      assertEq("[6]", asyncData2.project.content.reqs.size, 1)
//
//      val ves2 = svr.run(client2.createContent)(newUC).needRight.needNES
//      assertEq("[7]", recv1, Vector(ves1, ves2))
//      assertEq("[8]", recv2, Vector(ves2)) // Because BroadcastTo.All
//
//      val ves3 = svr.run(client1.createContent)(newUC).needRight.needNES
//      assertEq("[9]", recv1, Vector(ves1, ves2, ves3)) // Because BroadcastTo.All
//      assertEq("[A]", recv2, Vector(ves2, ves3))
//
//      logic.unregister(regId1).value
//      val ves4 = svr.run(client2.createContent)(newUC).needRight.needNES
//      assertEq("[B]", recv1, Vector(ves1, ves2, ves3))
//      assertEq("[C]", recv2, Vector(ves2, ves3, ves4))
//    }

//    'changesAfterUnregister {
//      val t = new Tester; import t._
//      db.addProject(pid, uid)()
//
//      var recv = Vector.empty[VerifiedEvent.NonEmptySeq]
//      val regId = logic.register(pid, uid, ve => Name(recv :+= ve)).value.needRight
//      val client = logic.initialClient(regId, usr).value.needRight
//      svr.run(client.initAsync)(()).needRight
//      logic.unregister(regId).value
//      assertEq(svr.run(client.createContent)(newUC), -\/(ProjectServer.NotRegistered.errorMsg))
//      assertEq(recv, Vector.empty)
//    }

  }
}
