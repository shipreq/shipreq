package shipreq.webapp.server.logic

import java.util.concurrent.ConcurrentHashMap
import scalaz.Scalaz.Id
import scalaz.{-\/, NaturalTransformation, ~>}
import utest._
import shipreq.taskman.api.UserId
import shipreq.webapp.base.test.WebappTestUtil._

object ProjectServerTest extends TestSuite {

  class Tester {
    implicit val storeMap: ProjectServer.StoreMap[Id, ConcurrentHashMap] = new ConcurrentHashMap()
    implicit val store: ProjectServer.StoreAlgebra[Id] = Store.Algebra.concurrentHashMap(storeMap)
    implicit val svr = new MockSvr
    implicit val db = new MockDb
    implicit val idToId: Id ~> Id = NaturalTransformation.id
    val logic = ProjectServer[Id, Id](ProjectServer.BroadcastTo.All)
  }

  val pid = ProjectId(100)
  val pid2 = ProjectId(101)
  val uid = UserId(200)
  val uid2 = UserId(201)

  override def tests = TestSuite {

    'registrationAndLoading {
      val t = new Tester; import t._
      db.addProject(pid, uid)()
      def test(storeSize: Int, dbLoadM: Int, dbLoadE: Int): Unit = {
        assertEq("store size", storeMap.size, storeSize)
        db.assertLoadCounts(dbLoadM, dbLoadE)
      }

      test(0, 0, 0); val regId1 = logic.register(pid, uid, _ => ()).needRight
      test(1, 1, 1); val regId2 = logic.register(pid, uid, _ => ()).needRight
      test(1, 1, 1); logic.unregister(regId1)
      test(1, 1, 1); val regId3 = logic.register(pid, uid, _ => ()).needRight
      test(1, 1, 1); logic.unregister(regId3)
      test(1, 1, 1); logic.unregister(regId3) // ignored
      test(1, 1, 1); logic.unregister(regId2)
      test(0, 1, 1); val regId4 = logic.register(pid, uid, _ => ()).needRight
      test(1, 2, 2)
    }

    'registerNoProject {
      val t = new Tester; import t._
      assertEq(logic.register(pid, uid, _ => ()), -\/(ProjectServer.ProjectNotFound))
    }

    'registerNotOwner {
      val t = new Tester; import t._
      db.addProject(pid, uid2)()
      def test() = assertEq(logic.register(pid, uid, _ => ()), -\/(ProjectServer.AccessDenied))
      test()
      logic.register(pid, uid2, _ => ()).needRight // Load with correct user so project is in store
      test()
    }

//    'updatesAndListeners {
//    }

  }
}
